# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#loading libraries
library(ggplot2)
library(likert)
library(png)
library(patchwork)
library(dplyr)
library(sjmisc)
library(stringr)
library(surveytoolbox)


# ordering möglichkeiten im Facet-wrap verbessern
# anzeige gruppen in einem facet und anzeige nur einer gruppe im facet und die anderen gruppen im anderen facet
# facetwrap horizhontal vertical bzw. mit ncol!!!
# diverged vs stacked bar chart   https://jakec007.github.io/2021-06-23-R-likert/
# dataframe kann jeden Namne haben
# info % direkt hinter Balken // x-Achse // aligned
# Plotting nice likert graphs ... drop function und line break on axis //  colour numbers in dependence of level number // dealing no answer categories // sample size //grouping ok but size of categories
# when grouping, grouping names groesser
# dreier, siebener, neuner (auch gerade???)
# categories vs category var? eine raus machen
# andere Darstelung wo alles von links nach rechts
# linerbreaker bei den labels? schon implementiert wie es aussieht
# sample size integration
# drop selected variables ? bei included
# likert mit na/do not know!!!!
# alle werte oder nur extremA
# input included="c(1,2,3,4)

plot_likert_graph <- function(variablef,picname="likert_graph",width_graph=11000,height_graph=6000,linebreaker=40,resolution_graphs=300,
                              textsize=40,rowslegend=1,drop_item=0,reverso=0,categories=0,category_var="",centering=FALSE,included="") {
  categories
  category_var
  textsize
  resolution_graphs
  reverso
  centering
  included
  centering[centering==TRUE]<-TRUE
  centering[centering==FALSE]<-FALSE
  # variablef<-"IM03_"
  items <- data[,substr(names(data), 1,5) == variablef] # was passier thier?
  number_items<-length(colnames(items))
  etc<-(0)

  # was ist comment -> siehe SoSci comment aus SoSci ziehen
  test_string<-comment(eval(parse(text=paste0("data$",variablef,number_items)))) # das funktioniert wohl nicht da muss noch eine null for number_item!!
  test_string<-str_contains(test_string,"input") # fuer sonstiges weil da input genommen wird -> thus we can delete it from the dataset
  etc[test_string==TRUE]<-1
  etc<-etc+drop_item
  number_levels<-length(levels(as.factor(items[,1])))

  # define number colors (hier machen wir das bis 100 diskret oder mit continours colorscales, so dass man es bis 100 machen kann)
  colorchoice11<-c("#000075", "#5c5cff","#A2CBFF","#E5FCFC","#E5FCFC" ,"#D7D5D3","#E5FCFC" ,"#E5FCFC","#BBF9BD", "#93cf6e","#0fa460")
  colorchoice10<-c("#000075", "#5c5cff","#A2CBFF","#E5FCFC","#E5FCFC" ,"#E5FCFC" ,"#E5FCFC","#BBF9BD", "#93cf6e","#0fa460")
  colorchoice9<-c("#000075", "#5c5cff","#A2CBFF","#E5FCFC" ,"#D7D5D3" ,"#EEFAEF","#BBF9BD", "#93cf6e","#0fa460")
  colorchoice8<-c("#000075", "#5c5cff","#A2CBFF","#E5FCFC","#EEFAEF","#BBF9BD", "#93cf6e","#0fa460")
  colorchoice7<-c("#000075", "#5c5cff","#A2CBFF" ,"#D7D5D3" ,"#BBF9BD", "#93cf6e","#0fa460")
  colorchoice6<-c("#000075", "#5c5cff","#A2CBFF" ,"#BBF9BD", "#93cf6e","#0fa460")
  colorchoice5<-c("#000075", "#5c5cff" ,"#D7D5D3" ,"#93cf6e","#0fa460")
  colorchoice4<-c("#000075", "#5c5cff","#93cf6e","#0fa460")
  colorchoice3<-c("#000075", "#D7D5D3" ,"#0fa460")
  colorchoice2<-c("#000075", "#0fa460")


  if (number_levels==7) {
    colorchoice<-c("#000075", "#5c5cff","#A2CBFF" ,"#D7D5D3" ,"#BBF9BD", "#93cf6e","#0fa460")
  } else if (number_levels==5) {
    colorchoice<-c("#000075", "#5c5cff" ,"#D7D5D3" ,"#93cf6e","#0fa460")
  } else if (number_levels==4) {
    colorchoice<-c("#000075", "#5c5cff","#93cf6e","#0fa460")
  } else if (number_levels==9) {
    colorchoice<-c("#000075", "#5c5cff","#A2CBFF","#E5FCFC" ,"#D7D5D3" ,"#EEFAEF","#BBF9BD", "#93cf6e","#0fa460")
  }

  #
  items <- as.data.frame(items)
  items <- items[,c(1:(length(items[1,])-etc))] # Auswahl der Items -> begrenzen wenn z.B. Sonstiges oder Quality Check drin ist
  items_list<-names(items)
  items_prepared_frame<-items
  items_names<-c(1:length(items[1,]))
  liste_names<-c(1:length(items[1,]))
  labellings<-as.data.frame(lapply(items, attributes)[1])
  labellings<-labellings[1,1:number_levels]
  mylevels <-labellings
  #number_levels<-7
  #
  num_label_level<-""
  num_label_level[number_levels==9]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4],
                                          items_prepared_frame[i]==5 ~ labellings[1,5],
                                       items_prepared_frame[i]==6 ~ labellings[1,6],
                                       items_prepared_frame[i]==7 ~ labellings[1,7],
                                       items_prepared_frame[i]==8 ~ labellings[1,8],
                                      items_prepared_frame[i]==9 ~ labellings[1,9])'
  num_label_level[number_levels==8]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4],
                                          items_prepared_frame[i]==5 ~ labellings[1,5],
                                       items_prepared_frame[i]==6 ~ labellings[1,6],
                                       items_prepared_frame[i]==7 ~ labellings[1,7],
                                       items_prepared_frame[i]==8 ~ labellings[1,8])'
  num_label_level[number_levels==7]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4],
                                          items_prepared_frame[i]==5 ~ labellings[1,5],
                                       items_prepared_frame[i]==6 ~ labellings[1,6],
                                       items_prepared_frame[i]==7 ~ labellings[1,7])'
  num_label_level[number_levels==6]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4],
                                          items_prepared_frame[i]==5 ~ labellings[1,5],
                                       items_prepared_frame[i]==6 ~ labellings[1,6])'
  num_label_level[number_levels==5]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4],
                                          items_prepared_frame[i]==5 ~ labellings[1,5])'
  num_label_level[number_levels==4]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3],
                                           items_prepared_frame[i]==4 ~ labellings[1,4])'
  num_label_level[number_levels==3]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2],
                                          items_prepared_frame[i]==3 ~ labellings[1,3])'
  num_label_level[number_levels==2]<-'case_when(items_prepared_frame[i]==1 ~ labellings[1,1],
                                             items_prepared_frame[i]==2 ~ labellings[1,2])'

  num_label_level<-gsub('[\t\n]', "",num_label_level)
  num_label_level<- gsub(
    pattern = ('\"'),
    replacement = '',
    x = num_label_level,
    fixed = T
  )
  num_label_level<-gsub('                                             ', "",num_label_level)
  num_label_level<-gsub('                                          ', "",num_label_level)
  num_label_level<-gsub('                                       ', "",num_label_level)
  num_label_level<-gsub('                                      ', "",num_label_level)

  ## recode factor befehl che ken
  i<-1
  for(i in 1:length(items[1,])) {
    items_prepared_frame[,i]<- assign(paste0("items$",items_list[i]), eval(parse(text=(num_label_level))))
  }

  i<-1
  while(i<length(items[1,])+1) {
    items_names[i]<-comment(eval(parse(text=paste0("data$",items_list[i]))))
    i<-i+1
  }
  i<-1
  while(i<length(items[1,])+1) {
    liste_names[i]<-sub(".*\\:", "", items_names[i])   #### rausnehmen wenn : drin bzw. bis dahin, scheint schematisch in SoSci
    i<-i+1
  }
  colnames(items_prepared_frame)<-liste_names
  # workaround missing values (source?)
  sapply(items_prepared_frame, class) #Verify that all the columns are indeed factors
  sapply(items_prepared_frame, function(x) { length(levels(x)) } ) # The number of levels in each factor
  # Here we will recode each factor and explicitly set the levels
  for(i in seq_along(items_prepared_frame)) {
    items_prepared_frame[,i] <- factor(items_prepared_frame[,i], levels=mylevels)
  }
  # reverse if necessary


  #items_prepared_frame<-reverse.levels(items_prepared_frame)

  items_prepared_frame[reverso==1]<-reverse.levels(items_prepared_frame)
  #  include variables only
  #included<-"c(1,3)"

  #included<-as.character(included)
  list <-as.character("c(1:length(items_prepared_frame[1,]))")
  if(included=="" ){
    varia<-list
  } else {
    varia<-as.character(included)
  }
  #items_prepared_frame<-items_prepared_frame[,eval(parse(text=varia))]
  items_prepared_frame=items_prepared_frame[,eval(parse(text=varia)), drop=FALSE]
  #exclude

  #grouping
  placer_1<-""
  placer_1[categories==1]<-"grouping=data$"
  placer_2<-""
  placer_2[categories==1]<-category_var
  likertscale<-eval(parse(text=paste0("likert(items_prepared_frame, ", placer_1,placer_2,')')))

  # Chi-Square-Test for groups
  #print(summary(likertscale)[order(summary(likertscale)[,2]),])
  # chi_output<-chisq.test(summary(likertscale)[order(summary(likertscale)[,c(3,4,5)])
  # # species <- unique(df$species)
  #
  #chi_cluster<- lapply(cluster, function(x) xtabs(count~trap, df,
  #
  #rownames(chi_output$residuals) <- summary(likertscale)[,1] # oder 2
  #library(corrplot)
  #corrplot(chi_output$residuals, is.cor = FALSE, method = "number")
  #print(chi_output$residuals)
  # Here single items can/are dropped
  # alternativ testen mit likert_barplot wg. einfluss auf
  # centered or not, dann müssen wohl Daten gedreht werden

  # Likert-Graphic
  likert_graphic<- plot(likertscale, centered = centering, text.size=textsize*.35, colors= colorchoice ) +
    theme(axis.text = element_text(size=textsize))
  likert_graphic<-likert_graphic +
    theme(legend.text = element_text(size=textsize),axis.title.x = element_text(size=textsize),
          axis.text.y = element_text(size=textsize,color = "black"), axis.text.x = element_text(size=textsize,color = "black"),
          strip.text = element_text(size = textsize)
    )+
    guides(fill=guide_legend(title=" ",nrow=rowslegend,byrow=TRUE))+
    labs(y = "%")+
    scale_x_discrete(position = "top",
                     labels = function(x) str_wrap(str_replace_all(x, "foo" , "foo"),width = linebreaker))+
    theme(plot.margin = unit(c(1,1,1, 1), "lines"))
  png_graph <- likert_graphic


  #  +    facet_wrap(~country,ncol=3)          das noch über ncol integrieren!!!!

  # Fit plot to image size for print on DINA4 and publication to web - which dpis, second row when long text for legend
  setwd(sub('/3_DATA','/5_GRAPHICS',getwd()))
  png(file= paste0(picname,".png"),width=width_graph, height=height_graph,   res = resolution_graphs)
  print(png_graph)
  dev.off()
  #setwd(sub('/3_DATA','/5_GRAPHICS',getwd()))
  #png(file= "significances.png",width=4000, height=height_graph,   res = resolution_graphs)
  #print(significances)
  #dev.off()
}
