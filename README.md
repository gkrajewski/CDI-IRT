Forked from [pikrol/CDI-IRT:master](https://github.com/pikrol/CDI-IRT)

There are two types of files[^note1] in the main directory:

- `fit-[language]-[scale].Rmd` (fitting the IRT model)
- `sim-[language]-[scale].Rmd` (analyses and simulations based on the IRT model)

[^note1]: Optionally, there might be corresponding knitted `html` files as well.

They can and **should be treated as templates**. To use with another language and/or scale:

1. copy, rename and modify the preamble and the settings chunk as instructed in the file;
2. for `fit-*.Rmd` to work, you need data as described in the file;
3. for `sim-*.Rmd` to work, you have to successfully knit corresponding `fit-*.Rmd` first;
3. you shouldn't need to modify anything else in the files.

`Functions` directory contains required function definitions.

`Data` directory is used for saving and loading data (raw CDI, interim & final models, simulations etc.).[^note2]
 
[^note2]: It's gitignored because of the size of `RData` files and to avoid issues with data sharing.

`Archive` directory contains lots of old and possibly still useful and/or interesting stuff.
Content in all its subdirectories other than `Archive/Rmd_2_0` comes from
the [forked pikrol/CDI-IRT](https://github.com/pikrol/CDI-IRT).
