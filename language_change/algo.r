# In order for an innovation to spread and become the 
# new norm in a language community, it must pass “a threshold 
# of frequency” (ibid).
# -> functional selection or social selection
options(warn=-1)
library(igraph)
library(netdiffuseR)
library(diffusr)
library(network)

number_of_nodes = 500
number_of_runs = 20
rewiring_p = 0.01
ratio_learner = 0.4
functional_bias = 20
number_of_innovator = 2
degree = 20
prob_for_random = degree*number_of_nodes/(number_of_nodes*(number_of_nodes-1))

simulation <- function(g){
  number_of_changed = c() 
  for (i in 1:number_of_runs){
    new_state = c() 
    for (node in V(g)){
      if (V(g)$age[node]<=2){ #learner  
        if (compute_fitness(g, node)>0){
          new_state <- c(new_state, 1)
        }else{
          new_state <- c(new_state, -1)
        }
      }else{ #adult
        new_state <- c(new_state, V(g)$state[node])
      }
    }
    print(table(V(g)$age))
    V(g)$state <- new_state
    V(g)$age <- V(g)$age%%5+1
    number_of_changed = c(number_of_changed,c(table(V(g)$state)["-1"]/number_of_nodes))
    print(table(V(g)$state))
  }
  return(number_of_changed)
}

compute_fitness <- function(g, v){ #positive then unchanged 
  neighbors = V(g)$state[neighbors(g, v)]>0
  sum <- length(neighbors[neighbors==TRUE])- length(neighbors[neighbors==FALSE])*functional_bias
  return(sum)
}

## Regular 
set.seed(1)
regular <- sample_k_regular(number_of_nodes, degree, directed = FALSE, multiple = FALSE)
V(regular)$age <- c(sample(1:2,number_of_nodes*ratio_learner,replace=T), sample(3:5,number_of_nodes*(1-ratio_learner)-number_of_innovator,replace=T), 3)
V(regular)$state <- c(rep(1, number_of_nodes-number_of_innovator),rep(-1, number_of_innovator))
table(V(regular)$age)
plot(regular)
plot(simulation(regular), type = "o", ylabel = "percentaget of C")

# random 
random <- erdos.renyi.game(number_of_nodes, p.or.m = prob_for_random)
V(random)$age <- c(sample(1:2,number_of_nodes*ratio_learner,replace=T), sample(3:5,number_of_nodes*(1-ratio_learner)-number_of_innovator,replace=T), 3)
V(random)$state <- c(rep(1, number_of_nodes-number_of_innovator),rep(-1, number_of_innovator))
table(V(random)$age)
plot(random)
plot(simulation(random), type = "o", ylabel = "percentaget of C")

## small-world Graph 
set.seed(1)
small_world <- sample_smallworld(1, number_of_nodes, degree/2, rewiring_p)
#plot(small_world)
V(small_world)$age <- c(sample(1:2,number_of_nodes*ratio_learner,replace=T), sample(3:5,number_of_nodes*(1-ratio_learner)-number_of_innovator,replace=T), 3)
V(small_world)$state <- c(rep(1, number_of_nodes-number_of_innovator),rep(-1, number_of_innovator))
plot(simulation(small_world), type = "o", ylabel = "percentaget of C")

# Scale-free  
scale_free <- sample_pa(number_of_nodes,m = degree/2, directed=FALSE)
V(scale_free)$age <- c(sample(1:2,number_of_nodes*ratio_learner,replace=T), sample(3:5,number_of_nodes*(1-ratio_learner)-number_of_innovator,replace=T), 3)
V(scale_free)$state <- c(rep(1, number_of_nodes-number_of_innovator),rep(-1, number_of_innovator))
table(V(scale_free)$age)
plot(scale_free)
plot(simulation(scale_free), type = "o", ylabel = "percentaget of C")
