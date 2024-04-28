The project can be built and tested on Linux/Mac OS environments. So far we haven't tried to build it on a Windows machine. 

Since the project is written in Ocaml, it had dependencies to opam, Ocaml 4.14. 
Since it uses Z3 as part of the back-end solving, it requires Z3. 
For the parsing purposes, it requires menhir. 

Building the project from scratch takes 2-3 hours time, due to the infrastructure inherited from Meta [Infer](http://fbinfer.com/) tool. We recommend the usage of the docker image, which has been built with all the setups, for both the tool and the benchmark projects. 
The detailed documentation for the docker is in [Artifact Evaluation Doc](ProveNFix_Artifact_Evaluation.pdf). 


