Forked from [pikrol/CDI-IRT:master](https://github.com/pikrol/CDI-IRT)

Currently the repository lets you do two things:

- run IRT model fitting (with `fit-template.Rmd`)
- run a set of analyses and simulations using such a model (with `sim-template.Rmd`)

`fit-template.Rmd` needs the relevant data (see the file for details) to work.

`sim-template.Rmd` needs the successful run of `fit-template.Rmd` on the relevant data to work.

You shouldn't (have to) modify anything in the files (unless a chunk explicitly states otherwise);
you'll be asked for the data you want to run them on, when you try running or knitting them.

Both files contain potentially time-consuming chunks. These chunks show their progress in the console
(if possible) and their results are saved, so that next time you run the code
the results are loaded, rather than recomputed.

Knitting is not allowed until you first run the whole code interactively,
so that you can observe all those progress bars and other important messages,
which would be difficult during the knitting process.
Also, by default, knitting is allowed only by running `knit_from_template.R`
(it will ask for necessary details[^batch_knitting]), which takes care of the output file's name
and location.[^manual_knitting]

[^manual_knitting]: You can use RStudio "knitting with parameters"
but the generated report file won't be properly named and placed.

[^batch_knitting]: `knit_from_template.R` accepts command line arguments as well, which can be useful for batch
processing (see `batch_example.sh`), but be careful with them --- there is no error handling whatsoever.

Subdirectories:

- `Functions` directory contains required function definitions.

- `Reports` directory contains knitted reports.[^reports]

- `Data` directory is used for saving and loading data (raw CDI, interim & final models, simulations etc.).[^data]

[^reports]: `knit_from_template.R` by default saves them there.

[^data]: It's gitignored because of the size of `RData` files and to avoid issues with data sharing.

- `Archive` directory contains lots of old and possibly still useful and/or interesting stuff.
Content in all its subdirectories other than `Archive/Rmd_2_0` comes from
the forked [pikrol/CDI-IRT](https://github.com/pikrol/CDI-IRT).
