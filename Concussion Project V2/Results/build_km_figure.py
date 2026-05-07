#!/usr/bin/env python3
"""
Publication-ready 2x2 Kaplan-Meier cumulative incidence figure for
CX1 vs CTL-FX over 5 years.

Supports:
- long-format CSV input
- directory of per-outcome CSVs
- demo synthetic data
- direct parsing of TriNetX KM exports for the active CX1 study
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
from matplotlib.lines import Line2D
from matplotlib.ticker import FuncFormatter


OUTCOME_ORDER = ["mood", "anxiety", "adhd", "sud"]
OUTCOME_META = {
    "mood": {
        "panel": "A",
        "title": "Mood disorders (F30-F39)",
        "trinetx_outcome": 4,
    },
    "anxiety": {
        "panel": "B",
        "title": "Anxiety and related disorders (F40-F48)",
        "trinetx_outcome": 5,
    },
    "adhd": {
        "panel": "C",
        "title": "Attention-deficit/hyperactivity disorder (F90)",
        "trinetx_outcome": 6,
    },
    "sud": {
        "panel": "D",
        "title": "Generalized substance use disorder (F10-F19)",
        "trinetx_outcome": 1,
    },
}

PANEL_STATS = {
    "mood": {
        "hr": 2.361891,
        "hr_lo": 2.257915,
        "hr_hi": 2.470655,
        "logrank_p": 0.0,
        "cx1_5y": 0.0912102931837386,
        "ctl_5y": 0.03982796344047699,
    },
    "anxiety": {
        "hr": 2.408115,
        "hr_lo": 2.326864,
        "hr_hi": 2.492203,
        "logrank_p": 0.0,
        "cx1_5y": 0.1562954956296505,
        "ctl_5y": 0.0686548983004985,
    },
    "adhd": {
        "hr": 2.700015,
        "hr_lo": 2.581169,
        "hr_hi": 2.824332,
        "logrank_p": 0.0,
        "cx1_5y": 0.0997339075875449,
        "ctl_5y": 0.0384081174499106,
    },
    "sud": {
        "hr": 1.731749,
        "hr_lo": 1.622110,
        "hr_hi": 1.848799,
        "logrank_p": 2.419159771052275e-62,
        "cx1_5y": 0.0356902667456912,
        "ctl_5y": 0.0207850074320275,
    },
}

COLORS = {"CX1": "#C8102E", "CTL-FX": "#1F4E79"}
X_TICKS = [0, 365, 730, 1095, 1460, 1825]
COMMON_YMAX = 0.20
COMMON_YTICKS = np.arange(0, COMMON_YMAX + 0.0001, 0.04)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build 2x2 flipped KM figure for CX1 vs CTL-FX.")
    parser.add_argument("--input", type=str, default=None, help="Long-format KM CSV or directory of per-outcome CSVs.")
    parser.add_argument("--stats", type=str, default=None, help="Optional panel stats CSV.")
    parser.add_argument("--output", type=str, default="figure2_panelB_km_cx1_vs_ctlfx", help="Output stem.")
    parser.add_argument("--dpi", type=int, default=600, help="Raster DPI.")
    parser.add_argument("--width", type=float, default=180, help="Figure width in mm.")
    parser.add_argument("--height", type=float, default=180, help="Figure height in mm.")
    parser.add_argument("--demo", action="store_true", help="Use synthetic data for layout testing.")
    parser.add_argument(
        "--trinetx-dir",
        type=str,
        default=None,
        help="Optional TriNetX export directory for CX1 vs CTL-FX. Overrides --input for direct parsing.",
    )
    parser.add_argument(
        "--write-extracted",
        action="store_true",
        help="Write parsed KM data and panel stats CSVs when using --trinetx-dir.",
    )
    return parser.parse_args()


def configure_matplotlib() -> None:
    mpl.rcParams.update(
        {
            "font.family": "sans-serif",
            "font.sans-serif": ["Arial", "Helvetica", "DejaVu Sans"],
            "font.size": 10,
            "axes.titlesize": 10,
            "axes.labelsize": 10,
            "legend.fontsize": 10,
            "xtick.labelsize": 9,
            "ytick.labelsize": 9,
        }
    )


def _validate_columns(df: pd.DataFrame, required: Iterable[str]) -> None:
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns: {missing}")


def _coerce_numeric(df: pd.DataFrame, columns: Iterable[str]) -> pd.DataFrame:
    for col in columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")
    return df


def _nearest_at_or_before(df: pd.DataFrame, time_point: int) -> pd.Series:
    subset = df.loc[df["time_days"] <= time_point]
    if subset.empty:
        return df.iloc[0]
    return subset.iloc[-1]


def generate_demo_data() -> Dict[str, pd.DataFrame]:
    n = 68375
    grid = np.array([0, 30, 60, 90, 180, 365, 540, 730, 910, 1095, 1280, 1460, 1645, 1825], dtype=int)
    targets = {
        "mood": (0.091, 0.040),
        "anxiety": (0.156, 0.069),
        "adhd": (0.100, 0.038),
        "sud": (0.036, 0.021),
    }
    out = {}
    for outcome, (cx1_target, ctl_target) in targets.items():
        frames: List[pd.DataFrame] = []
        for cohort, target in [("CX1", cx1_target), ("CTL-FX", ctl_target)]:
            alpha = 1.2 if outcome != "sud" else 1.5
            cum = target * (grid / 1825.0) ** alpha
            ci_pad = np.clip(0.004 + 0.015 * cum, 0.003, 0.018)
            n_at_risk = np.round(n * (1 - cum)).astype(int)
            frames.append(
                pd.DataFrame(
                    {
                        "outcome": outcome,
                        "cohort": cohort,
                        "time_days": grid,
                        "cum_incidence": cum,
                        "ci_lower": np.clip(cum - ci_pad, 0, 1),
                        "ci_upper": np.clip(cum + ci_pad, 0, 1),
                        "n_at_risk": n_at_risk,
                    }
                )
            )
        out[outcome] = pd.concat(frames, ignore_index=True)
    return out


def _read_trinetx_lines(path: Path) -> List[str]:
    lines = path.read_text(encoding="utf-8").splitlines()
    if lines and lines[0].startswith("\ufeff"):
        lines[0] = lines[0].lstrip("\ufeff")
    return [line.strip() for line in lines]


def _extract_csv_block(lines: List[str], header_line: str) -> pd.DataFrame:
    try:
        idx = lines.index(header_line)
    except ValueError as exc:
        raise ValueError(f"Header not found: {header_line}") from exc
    csv_lines = [lines[idx]]
    for line in lines[idx + 1 :]:
        if line == '" "' or line == "":
            break
        csv_lines.append(line)
    return pd.read_csv(pd.io.common.StringIO("\n".join(csv_lines)))


def parse_trinetx_km(trinetx_dir: Path) -> Tuple[Dict[str, pd.DataFrame], Dict[str, dict]]:
    data_dict: Dict[str, pd.DataFrame] = {}
    stats_dict: Dict[str, dict] = {}

    for outcome_key in OUTCOME_ORDER:
        outcome_num = OUTCOME_META[outcome_key]["trinetx_outcome"]
        graph_path = trinetx_dir / f"Outcome_{outcome_num}_Result_b_KM_graph.csv"
        table_path = trinetx_dir / f"Outcome_{outcome_num}_Result_b_KM_table.csv"

        graph_lines = _read_trinetx_lines(graph_path)
        graph_df = _extract_csv_block(
            graph_lines,
            "Time (Days),Cohort 1: Survival Probability,Cohort 1: Survival Probability 95 % CI Lower,Cohort 1: Survival Probability 95 % CI Upper,Cohort 2: Survival Probability,Cohort 2: Survival Probability 95 % CI Lower,Cohort 2: Survival Probability 95 % CI Upper",
        )
        graph_df = _coerce_numeric(graph_df, graph_df.columns)
        graph_df = graph_df.sort_values("Time (Days)").reset_index(drop=True)
        if graph_df["Time (Days)"].iloc[0] != 0:
            graph_df = pd.concat(
                [
                    pd.DataFrame(
                        {
                            "Time (Days)": [0],
                            "Cohort 1: Survival Probability": [1.0],
                            "Cohort 1: Survival Probability 95 % CI Lower": [1.0],
                            "Cohort 1: Survival Probability 95 % CI Upper": [1.0],
                            "Cohort 2: Survival Probability": [1.0],
                            "Cohort 2: Survival Probability 95 % CI Lower": [1.0],
                            "Cohort 2: Survival Probability 95 % CI Upper": [1.0],
                        }
                    ),
                    graph_df,
                ],
                ignore_index=True,
            )
        graph_df = graph_df.ffill()

        table_lines = _read_trinetx_lines(table_path)
        cohort_df = _extract_csv_block(
            table_lines,
            "Cohort,Cohort Name,Patients in Cohort,Patients with Outcome,Median Survival (Days),Survival Probability at End of Time Window",
        )
        cohort_df = _coerce_numeric(cohort_df, cohort_df.columns)
        logrank_df = _extract_csv_block(table_lines, "χ²,df,p")
        logrank_df = _coerce_numeric(logrank_df, logrank_df.columns)
        hr_df = _extract_csv_block(table_lines, "Hazard Ratio,95 % CI Lower,95 % CI Upper")
        hr_df = _coerce_numeric(hr_df, hr_df.columns)

        total_n = int(cohort_df["Patients in Cohort"].iloc[0])
        rows = []
        for cohort_label, prefix in [("CX1", "Cohort 1"), ("CTL-FX", "Cohort 2")]:
            surv = graph_df[f"{prefix}: Survival Probability"].to_numpy()
            lo = graph_df[f"{prefix}: Survival Probability 95 % CI Lower"].to_numpy()
            hi = graph_df[f"{prefix}: Survival Probability 95 % CI Upper"].to_numpy()
            cum = 1 - surv
            # TriNetX export does not include explicit risk set counts in the KM graph CSV.
            # As a fallback, estimate the displayed risk table from the event-free proportion.
            n_at_risk = np.rint(total_n * surv).astype(int)
            rows.append(
                pd.DataFrame(
                    {
                        "outcome": outcome_key,
                        "cohort": cohort_label,
                        "time_days": graph_df["Time (Days)"].astype(int),
                        "cum_incidence": cum,
                        "ci_lower": np.clip(1 - hi, 0, 1),
                        "ci_upper": np.clip(1 - lo, 0, 1),
                        "n_at_risk": n_at_risk,
                    }
                )
            )

        data_dict[outcome_key] = pd.concat(rows, ignore_index=True)
        stats_dict[outcome_key] = {
            "hr": float(hr_df["Hazard Ratio"].iloc[0]),
            "hr_lo": float(hr_df["95 % CI Lower"].iloc[0]),
            "hr_hi": float(hr_df["95 % CI Upper"].iloc[0]),
            "logrank_p": float(logrank_df["p"].iloc[0]),
            "cx1_5y": float(1 - cohort_df["Survival Probability at End of Time Window"].iloc[0]),
            "ctl_5y": float(1 - cohort_df["Survival Probability at End of Time Window"].iloc[1]),
        }

    return data_dict, stats_dict


def load_km_data(input_path: str | None, demo: bool = False, trinetx_dir: str | None = None) -> Dict[str, pd.DataFrame]:
    required = ["outcome", "cohort", "time_days", "cum_incidence", "ci_lower", "ci_upper", "n_at_risk"]
    if demo:
        return generate_demo_data()

    if trinetx_dir:
        data_dict, _ = parse_trinetx_km(Path(trinetx_dir))
        return data_dict

    if not input_path:
        raise ValueError("Provide --input, --trinetx-dir, or --demo.")

    path = Path(input_path)
    if path.is_dir():
        frames = []
        for csv_path in sorted(path.glob("*.csv")):
            frame = pd.read_csv(csv_path)
            frames.append(frame)
        df = pd.concat(frames, ignore_index=True)
    else:
        df = pd.read_csv(path)

    _validate_columns(df, required)
    df = _coerce_numeric(df, ["time_days", "cum_incidence", "ci_lower", "ci_upper", "n_at_risk"])
    out = {}
    for outcome in OUTCOME_ORDER:
        sub = df.loc[df["outcome"] == outcome].copy()
        if sub.empty:
            raise ValueError(f"No data found for outcome '{outcome}'.")
        out[outcome] = sub.sort_values(["cohort", "time_days"]).reset_index(drop=True)
    return out


def load_stats(stats_path: str | None = None, trinetx_dir: str | None = None, demo: bool = False) -> Dict[str, dict]:
    if demo:
        return PANEL_STATS
    if trinetx_dir:
        _, stats_dict = parse_trinetx_km(Path(trinetx_dir))
        return stats_dict
    if not stats_path:
        return PANEL_STATS

    df = pd.read_csv(stats_path)
    _validate_columns(df, ["outcome", "hr", "hr_lo", "hr_hi", "logrank_p", "cx1_5y", "ctl_5y"])
    stats = {}
    for row in df.to_dict(orient="records"):
        stats[row["outcome"]] = row
    return stats


def render_risk_table(ax: plt.Axes, df: pd.DataFrame, x_ticks: List[int]) -> None:
    ax.set_xlim(0, 1825)
    ax.set_ylim(-0.2, 2.2)
    ax.axis("off")
    trans = mpl.transforms.blended_transform_factory(ax.transData, ax.transAxes)
    left_trans = mpl.transforms.blended_transform_factory(ax.transAxes, ax.transAxes)

    ax.text(-0.23, 0.83, "No. at risk", transform=left_trans, ha="left", va="center", fontsize=8.0, fontweight="bold")
    row_positions = {"CX1": 0.48, "CTL-FX": 0.14}
    label_colors = {"CX1": COLORS["CX1"], "CTL-FX": COLORS["CTL-FX"]}

    for cohort, ypos in row_positions.items():
        ax.text(-0.23, ypos, cohort, transform=left_trans, ha="left", va="center", fontsize=7.9, color=label_colors[cohort])
        cohort_df = df.loc[df["cohort"] == cohort].sort_values("time_days")
        for tick in x_ticks:
            row = _nearest_at_or_before(cohort_df, tick)
            ax.text(
                tick,
                ypos,
                f"{int(row['n_at_risk']):,}",
                transform=trans,
                ha="center",
                va="center",
                fontsize=7.8,
                color=label_colors[cohort],
            )


def format_p_value(value: float) -> str:
    if pd.isna(value):
        return "p = NA"
    if value < 0.001:
        return "p < 0.001"
    return f"p = {value:.3f}"


def plot_panel(
    fig: plt.Figure,
    gs_cell,
    outcome_key: str,
    data: pd.DataFrame,
    stats: dict,
    sharey_ax: plt.Axes | None = None,
) -> Tuple[plt.Axes, plt.Axes]:
    panel_spec = gs_cell.subgridspec(2, 1, height_ratios=[4.8, 1.45], hspace=0.06)
    ax = fig.add_subplot(panel_spec[0], sharey=sharey_ax)
    risk_ax = fig.add_subplot(panel_spec[1], sharex=ax)

    for cohort in ["CX1", "CTL-FX"]:
        cohort_df = data.loc[data["cohort"] == cohort].sort_values("time_days")
        ax.fill_between(
            cohort_df["time_days"].to_numpy(),
            cohort_df["ci_lower"].to_numpy(),
            cohort_df["ci_upper"].to_numpy(),
            step="post",
            alpha=0.20,
            color=COLORS[cohort],
            linewidth=0,
        )
        ax.step(
            cohort_df["time_days"].to_numpy(),
            cohort_df["cum_incidence"].to_numpy(),
            where="post",
            color=COLORS[cohort],
            linewidth=1.8,
        )

    ax.set_xlim(0, 1825)
    ax.set_ylim(0, COMMON_YMAX)
    ax.set_xticks(X_TICKS)
    ax.set_yticks(COMMON_YTICKS)
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, pos: f"{x:.0%}"))
    ax.grid(False)
    ax.yaxis.grid(True, color="#EEEEEE", linewidth=0.6)
    ax.tick_params(axis="both", direction="out", length=3, width=0.8, pad=2)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_linewidth(0.8)
    ax.spines["bottom"].set_linewidth(0.8)

    ax.text(0.01, 0.98, OUTCOME_META[outcome_key]["panel"], transform=ax.transAxes, ha="left", va="top", fontsize=12, fontweight="bold")
    ax.text(
        0.08,
        0.98,
        OUTCOME_META[outcome_key]["title"],
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10,
        fontweight="bold",
    )
    ax.text(
        0.98,
        0.09,
        f"HR {stats['hr']:.3f} ({stats['hr_lo']:.3f}, {stats['hr_hi']:.3f})\nLog-rank {format_p_value(stats['logrank_p'])}",
        transform=ax.transAxes,
        ha="right",
        va="bottom",
        fontsize=9,
    )

    render_risk_table(risk_ax, data, X_TICKS)
    return ax, risk_ax


def save_figure(fig: plt.Figure, output_stem: Path, dpi: int) -> None:
    fig.savefig(output_stem.with_suffix(".png"), dpi=dpi, bbox_inches="tight", facecolor="white")
    fig.savefig(output_stem.with_suffix(".svg"), bbox_inches="tight", facecolor="white")
    fig.savefig(output_stem.with_suffix(".pdf"), bbox_inches="tight", facecolor="white")


def main() -> None:
    args = parse_args()
    configure_matplotlib()

    km_data = load_km_data(args.input, demo=args.demo, trinetx_dir=args.trinetx_dir)
    panel_stats = load_stats(args.stats, trinetx_dir=args.trinetx_dir, demo=args.demo)

    if args.trinetx_dir and args.write_extracted:
        all_data = pd.concat([km_data[key] for key in OUTCOME_ORDER], ignore_index=True)
        all_data.to_csv("km_data_cx1_vs_ctlfx.csv", index=False)
        pd.DataFrame(
            [{"outcome": key, **panel_stats[key]} for key in OUTCOME_ORDER]
        ).to_csv("panel_stats_cx1_vs_ctlfx.csv", index=False)

    width_in = args.width / 25.4
    height_in = args.height / 25.4
    fig = plt.figure(figsize=(width_in, height_in), constrained_layout=False)
    outer = GridSpec(
        2,
        2,
        figure=fig,
        left=0.11,
        right=0.99,
        top=0.88,
        bottom=0.09,
        wspace=0.20,
        hspace=0.34,
    )

    top_axes = []
    sharey_ax = None
    for idx, outcome_key in enumerate(OUTCOME_ORDER):
        row, col = divmod(idx, 2)
        ax, risk_ax = plot_panel(
            fig,
            outer[row, col],
            outcome_key,
            km_data[outcome_key],
            panel_stats[outcome_key],
            sharey_ax=sharey_ax,
        )
        if sharey_ax is None:
            sharey_ax = ax
        if row == 1:
            ax.set_xlabel("")
            risk_ax.text(0.5, -0.28, "Days from index event", transform=risk_ax.transAxes, ha="center", va="top", fontsize=10)
        else:
            ax.set_xlabel("")
            plt.setp(ax.get_xticklabels(), visible=False)
        if col == 0:
            ax.set_ylabel("Cumulative incidence")
        else:
            ax.set_ylabel("")
            ax.tick_params(labelleft=True)
        top_axes.append(ax)

    legend_handles = [
        Line2D([0], [0], color=COLORS["CX1"], lw=1.8, label="CX1 (single concussion)"),
        Line2D([0], [0], color=COLORS["CTL-FX"], lw=1.8, label="CTL-FX (forearm fracture control)"),
    ]
    fig.legend(
        handles=legend_handles,
        loc="upper center",
        bbox_to_anchor=(0.5, 0.975),
        ncol=2,
        frameon=False,
        handlelength=2.5,
        columnspacing=1.2,
    )

    output_stem = Path(args.output)
    save_figure(fig, output_stem, args.dpi)
    plt.close(fig)


if __name__ == "__main__":
    main()
