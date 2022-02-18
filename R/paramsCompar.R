#' function allowing the comparison of multiple Mizer params object downloaded from tuneParams
#' need to compare species param and resource param on a plot
#'
#' instructions
# function that compares a bunch of param object and plot diff
# group by dates or zip name folder
#
# compare 2 successive objects in row e.g. 1 and 2, 2 and 3, so on
#
# add option to compare biomass change (run the sim from the param object to get final state) and size spectrums
#
# plots of 2 params objects need to overlap (differentiate them with alpha or line style)
#
# show only the parameters that changed
#
# do all the steps at once, output on an html file everything
#
# make a rmarkdown that does this
# need to specify where the zip files are
#' markdown need to look like:
#' table with different parameters
#' followed by biomass and size spectrum plot
#'
#' @param folder folder name where all the zip files are
#' @export

paramsCompar <- function(folder)
{
    # print(folder)
    myZips <- dir(folder)

    for(iZip in 1:(length(myZips)-1))
    {
        print("Files compared")
        print(myZips[iZip])
        print(myZips[iZip+1])
        # read objects 2 by 2

        #if params files are in zip form mixed with other png and stuff
        # object1 <- readRDS(gzcon(unz(paste(folder, myZips[iZip], sep = "/"),"tuned_params.RDS"))) #  gzcon() apply GZIP decompression to unz connection
        # object2 <- readRDS(gzcon(unz(paste(folder, myZips[iZip+1], sep = "/"),"tuned_params.RDS")))

        #if params files are all together in one folder
        object1 <- readRDS(paste(folder,"/tuned_params (",iZip,").rds", sep=""))
        object2 <- readRDS(paste(folder,"/tuned_params (",(iZip+1),").rds", sep=""))

        # print(object1@species_params)
        # print(object2@species_params)
        # read difference of species and resource parameters of the 2 objects
print("summary of differences")
        print(compareParams(object1,object2))
print("detailed species")
        # print(setdiff(object1@species_params[,1:30],object2@species_params[,1:30])) # tuneParams can add 4 columns of biomass observed. The diff function doesn't like it. Need to preformat the dataframe
        # print(setdiff(object1@resource_params,object2@resource_params))
        # print(setdiff(object1@other_params,object2@other_params))

# I want to print the detailed results only if there is an actual difference
sp_sum <- arsenal::comparedf(object1@species_params,object2@species_params)
sp_sum_values <- sp_sum$vars.summary$values # where the differences are read
sp_sum_values[[length(sp_sum_values)]] <- NULL # last slot is a character

if(dim(data.table::rbindlist(sp_sum_values))[1]) print(summary(arsenal::comparedf(object1@species_params,object2@species_params))) else print("no differences in species parameters")

print("detailed resource")
re_sum <- arsenal::comparedf(as.data.frame(object1@resource_params),as.data.frame(object2@resource_params))
re_sum_values <- re_sum$vars.summary$values # where the differences are read
re_sum_values[[length(re_sum_values)]] <- NULL # last slot is a character

if(dim(data.table::rbindlist(re_sum_values))[1]) print(summary(arsenal::comparedf(as.data.frame(object1@resource_params),as.data.frame(object2@resource_params)))) else print("no differences in resource parameters")
# print(arsenal::comparedf(as.data.frame(object1@resource_params),as.data.frame(object2@resource_params)))


print("detailed other resources")
ot_sum <- arsenal::comparedf(object1@other_params$other$MR$resource_params,object2@other_params$other$MR$resource_params)
ot_sum_values <- ot_sum$vars.summary$values # where the differences are read
ot_sum_values[[length(ot_sum_values)]] <- NULL # last slot is a character

if(dim(data.table::rbindlist(ot_sum_values))[1]) print(summary(arsenal::comparedf(object1@other_params$other$MR$resource_params,object2@other_params$other$MR$resource_params))) else print("no differences in other resources parameters")
# print(arsenal::comparedf(as.data.frame(object1@other_params),as.data.frame(object2@other_params)))
# print(summary(arsenal::comparedf(as.data.frame(object1@other_params),as.data.frame(object2@other_params))))

        # project to steady to be able to compare biomass
        steady1 <- projectToSteady(object1,progress_bar = F)
        sim1 <- project(steady1, t_max = 10,progress_bar = F)
        steady2 <- projectToSteady(object2,progress_bar = F)
        sim2 <- project(steady2, t_max = 10,progress_bar = F)

        #plot biomass difference between objects
        biom1 <- plotBiomass(sim1,return_data = T)
        biom2 <- plotBiomass(sim2,return_data = T)

        # biom1$state <- "previous"
        # biom2$state <- "current"
        #
        # plot_dat <- rbind(biom1,biom2)

        p1 <- ggplot(biom2) +
            geom_line(aes(x = Year, y = Biomass, color = Species)) +
            geom_line(data = biom1, aes(x = Year, y = Biomass, color = Species), linetype = "dashed", alpha = .3)

        #plot spectrum difference between objects
        spectra1 <- plotSpectra(sim1, return_data = T)
        spectra2 <- plotSpectra(sim2, return_data = T)

        p2 <- ggplot(spectra2) +
            geom_line(aes(x = w, y = value, color = Species)) +
            geom_line(data = spectra1, aes(x = w, y = value, color = Species), linetype = "dashed", alpha = .3) +
            scale_x_continuous(trans = "log10") +
            scale_y_continuous(trans = "log10")

        print("size spectrum comparison")
        print(p2)
print("biomass comparison")
        print(p1)
print(ggplot()) # just trying to get the biomass plot to appear after the size spectrum and not in the next iteration of the loop
        print("comparison ends")

    }
}


#' Function to generate the html with paramsCompar output
#'
#' @param folder Character string pointing to the folder
#' where the params object to compare are stored
#' @export

getDiffParams <- function(folder)
{
rmarkdown::render(input = "inst/comparisonTuneParams.Rmd", params = list(folder =folder))
}
