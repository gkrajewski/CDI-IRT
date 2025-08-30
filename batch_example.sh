#!/bin/bash
for type in "fit" "sim"; do
    for lang in "Norwegian" "Polish"; do
        for scale in "WG Comprehension" "WG Production" "WG Gestures" "WS"; do
            Rscript knit_from_template.R "$type" "$lang" "$scale"
        done
    done
done
