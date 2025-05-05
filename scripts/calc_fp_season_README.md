## Fantasy Football Points Calculation

This function calculates fantasy football points using nflfastR play-by-play data.

### Function: `calc_fantasy_fastR`

- **season**: NFL season year (e.g., 2023)
- **use_tiered_ppr**: Use tiered PPR scoring (default = TRUE)
- **te_premium**: Add 0.5 points per TE reception (default = TRUE)
- **rush_att_bonus**: Points per rushing attempt (default = 0.25)
- **pass_yd, pass_td, pass_int, pick6_penalty**: Standard passing scoring
- **rush_yd, rush_td**: Rushing scoring
- **rec_yd, rec_td, ppr**: Standard reception scoring (not used if tiered = TRUE)
- **fumbles**: Points lost per fumble (default = -2)

### Example Usage:
```R
result <- calc_fantasy_fastR(season = 2023, use_tiered_ppr = TRUE, te_premium = TRUE)
