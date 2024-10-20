using Pkg;
isinstalled(pkg::String) = any(x -> x.name == pkg && x.is_direct_dep, values(Pkg.dependencies()));
if !isinstalled("SFrontiers")
  redirect_stderr(stdout) do
  	Pkg.add("SFrontiers")
  end
end
using SFrontiers;
if !isinstalled("LinearAlgebra")
  redirect_stderr(stdout) do
    Pkg.add("LinearAlgebra")
  end
end
using LinearAlgebra;

# inputs
#y = [1,2,3,4,5,6,7,8,9,10]
#x = [1.1 1.2 1.3;2.1 2.2 2.3;1.2 1.5 1.6;1.7 1.4 5.6;1.5 5.7 2.6;5.7 3.6 5.1;5.4 6.1 7.4;3.6 3.6 3.5;7.8 4.6 3.1;5.1 3.2 6.3]
#cons = [1,1,1,1,1,1,1,1,1,1]

# https://discourse.julialang.org/t/remove-identical-columns-from-matrix/62378/5
# normalization does not change the final efficiency scores
xNormalized = hcat(unique(normalize.(eachcol(x)))...)

# run
sfmodel_spec(sftype(prod), sfdist(half), depvar(y), frontier(xNormalized), sigma_u_2(cons), sigma_v_2(cons))
sfmodel_opt(verbose(false), banner(false), marginal(false))
res = sfmodel_fit()

# output between 0 and 1 as efficiency score (Battese and Coelli (1988) efficiency index)
#res.bc