clear all
clc


%First of all, we create the sets which are;
    % N or n represents the number of data points
    % orig_M or d represents total number of features
    % p represents the number of clusters 
    % q represents the number of features that is selected in each cluster 
    
ga_summary=[];
p = 2; % number of clusters in each parent


N=[80,100,200]


Set_of_M = [5,6,8];




param_mutation_probability=0.03; %mutasyona girme olasılığını da buradan giriyoruz


%We set the number of repetions for each [m,p,q,n] set
ga_number_of_repetition=50;


%We set the number of generations that will be created for each [m,p,q,n] set
param_number_of_generations = 100;

%We determine the ratio of type 1 initialization
param_init_random_ratio=1;

%We determine the file name that we will write our results
ga_filename = ['results_p', num2str(p),'_Mut','.xlsx'];

for iiii=1:length(N)
    n=N(iiii);
    % we set population size 
    %population size is selected as the number of data points
    
    param_population_size=n
    
    for jjj=1:length(Set_of_M)
        orig_m=Set_of_M(jjj);
        d=orig_m;
        for q=2
            % we clear the variables that are not required for the
            % future usage
            clearvars -except ga_* q p n Set_of_M d orig_m orig_M N param_*
            
            %% Data Input Start
            %Save current path cd as "pathh"
            pathh = cd;
            [data, ga_Sheet_Name] = read_data(pathh,p,n,orig_m,q);
            % Data Input End
            
            %% Start of GA
            
            for ga_algorithm_repetition=1:ga_number_of_repetition
                
                % we clear the variables that are not required for the
                % future usage
                clearvars -except ga_* param_* data* q p n d Set_of_M orig_m orig_M N feature_indexes ;
                
                tic % we save the start time and do it for each ga repetiton
                %% Initializations  - Start
                % generation of the Initial population
                
                parent_features=[];parent_centers=[];
                
                % we create initial parents
                
                if param_init_random_ratio~=0
                    init_rand_start=1; init_rand_end=ceil(param_population_size*param_init_random_ratio);
                    [init_rand_infeasibility_count,parent_features,parent_centers ] = random_initialization(p,d,q,param_population_size,n,data,init_rand_start,init_rand_end);
                end
                
                % Initializations end
                %% parent selection for the first generation
                
                for i=1:param_population_size
                    [fitness_set(i), clusters] = cluster_update(data,parent_features(i,:),parent_centers(i,:),p,n,d);
                    current_population_feature_set(i,:) = parent_features(i,:);
                    current_population_center_set(i,:) = parent_centers(i,:);
                end
                
                %initial population is sorted for the first iteration
                [ordering index] = sort(fitness_set,'ascend');
                new_parent_feature_set_temp = current_population_feature_set(index,:);
                new_parent_center_set_temp = current_population_center_set(index,:);
                
                new_parent_feature_set = new_parent_feature_set_temp (1:param_population_size,:);
                new_parent_center_set = new_parent_center_set_temp (1:param_population_size,:);
                
                %% start of Generations
                generation_index=0;
                ind=0;
                
                while generation_index < param_number_of_generations && ind~=1
                    generation_index=generation_index+1;
                    ind=1;
                    
                    %% do cross-over operation for all parents
                    %[ind,child_centers,child_features]= crossover(ind,p,d,population_size,feature_indexes,new_parent_feature_set,new_parent_center_set);
                    %burada crossover'u çağırırız uniform yerine
                    [ind,child_centers,child_features]= crossover(ind,p,d,param_population_size,new_parent_feature_set,new_parent_center_set);
                    % Cocuklar oluştuktan sonra, parentlarla birleştirerek
                    % yeni populasyonumuzu oluşturmuş oluyoruz.
                    current_population_feature_set=[]; current_population_center_set=[];
                    current_population_feature_set= [new_parent_feature_set; child_features];
                    current_population_center_set = [new_parent_center_set; child_centers];
                    [current_inhabitants, columns_total1] = size(current_population_center_set);
                    
                    %%% Crossover - end
                    
                    %% Mutation - start
                    
                    % mutation operation
                    mutation_count=0;
 
                    for i=1:current_inhabitants
                        r=rand;
                        if r < param_mutation_probability
                            current_population_feature_set(i,:) = mutation(current_population_feature_set(i,:),p);
                            mutation_count=mutation_count+1;
                        end
                    end
                    
                    %%% Mutation - end
                    
                    %% fitness calculation
                    
                    fitness_set=[];
                    
                    for i=1:current_inhabitants
                        [fitness_set(i), cluster_set] = cluster_update(data,current_population_feature_set(i,:),current_population_center_set(i,:),p,n,d);
                    end
                    
                    %% sorting
                    [ordering, index] = sort(fitness_set,'ascend');
                    new_parent_feature_set_temp = current_population_feature_set(index,:);
                    new_parent_center_set_temp = current_population_center_set(index,:);
                    
                    %% offspring selection
                    
                    new_parent_features_set_1 = new_parent_feature_set_temp (1:floor(param_population_size/20),:);
                    new_parent_center_set_1 = new_parent_center_set_temp (1:floor(param_population_size/20),:);
                    
                    %Random olan kısmı ekleyelim
                    A = find(fitness_set~=100000 & fitness_set>ordering(floor(param_population_size/20)));
                    B = length(A);
                    C = randperm(B);
                    
                    new_random_parent_features_set_temp1 = current_population_feature_set(A(C),:);
                    new_random_parent_center_set_temp1  = current_population_center_set(A(C),:);
                    
                    new_random_parent_features_set_temp2 = new_random_parent_features_set_temp1(1:(param_population_size-floor(param_population_size/20)),:);
                    new_random_parent_center_set_temp2 = new_random_parent_center_set_temp1(1:(param_population_size-floor(param_population_size/20)),:);
                    
                    new_parent_feature_set = [new_parent_features_set_1; new_random_parent_features_set_temp2];
                    new_parent_center_set = [new_parent_center_set_1; new_random_parent_center_set_temp2];
                    

                    %% Value saving
                    bestfitness_set(generation_index) = ordering(1);
                    ga_bestfitness_values(ga_algorithm_repetition,generation_index)=min(fitness_set);
                    ga_worstfitness_values(ga_algorithm_repetition,generation_index)=max(fitness_set);
                    
                    bestparent(generation_index,:) = current_population_center_set(index(1),:);
                    number_of_mutations(generation_index) = mutation_count;
                  
                    
                end
                
                
                ga_duration(ga_algorithm_repetition) = toc ;
                %bestfitness_set(number_of_generations);
                %mean(bestfitness_set);
                [aa, bb] = min(bestfitness_set);
                ga_results(ga_algorithm_repetition)=aa;
                ga_indexes(ga_algorithm_repetition)=bb;
                ga_total_mutate(ga_algorithm_repetition) = sum(number_of_mutations);
                
            end
            
            %% In this subpart, we write our solutions to a MS Excel File
            
            Final = [ga_results' ga_indexes' ga_duration' ga_total_mutate' ];
            table_results = array2table(Final);
            
            
            writetable(table_results,ga_filename,'Sheet',ga_Sheet_Name);
            
            best_of_all=min(ga_results);
            worst_of_all=max(ga_results);
            ga_summary=[ga_summary; n, p, q, orig_m, best_of_all, worst_of_all, mean(ga_results), median(ga_results),mean(ga_duration), sum(ga_results==best_of_all), sum(ga_results==worst_of_all)];
            
        end
    end
end

writematrix(ga_summary,ga_filename,'Sheet','SummaryOfResults');

