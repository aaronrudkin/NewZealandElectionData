# Operation

`in.txt` includes the aggregated election results; `cabinet.txt` includes cabinet ministers; `nicknames.csv` provides nickname pairs for matching names across elections when the presentation of a name differs. `decisions.txt` contains manual coding decisions used by `processResults.py` in cases where it is unclear if two people with similar names are the same person.

Run `processResults.py` to merge all data files and panel-ize the election results.

If you want to manually re-code the manual coding decisions, delete `decisions.txt` and change `replay = 1` to `replay = 0` in `processResults.py`.

Results are stored in `output.csv`.
