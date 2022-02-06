function [ind,child_centers,child_features ] = crossover(ind,p,d,population_size,new_parent_feature_set,new_parent_center_set)

child_features=zeros(1,p*d);
child_centers=zeros(1,p);
for k=1:population_size/2
    temp1_features=new_parent_feature_set(k,:);
    temp2_features=new_parent_feature_set(population_size-k+1,:);
    temp1_centers=new_parent_center_set(k,:);
    temp2_centers=new_parent_center_set(population_size-k+1,:);
    
    %In the first part of the crossover, we cross only the features of two
    %parent
    
    child_temp_features1=[];
    child_temp_features2=[];

    for j=1:p
        
        if mod(j,2) == 1
            child_temp_features1(d*(j-1)+1:d*j) = temp1_features(d*(j-1)+1:d*j);
            child_temp_features2(d*(j-1)+1:d*j) = temp2_features(d*(j-1)+1:d*j);
            
        else
            child_temp_features1(d*(j-1)+1:d*j) = temp2_features(d*(j-1)+1:d*j);
            child_temp_features2(d*(j-1)+1:d*j) = temp1_features(d*(j-1)+1:d*j);
        end
    end
    
    if (sum((child_temp_features1) ~= (temp1_features))~=0) && (sum((child_temp_features1) ~= (temp2_features))~=0)
        child_centers(ind,:)=temp1_centers;
        child_features(ind,:)= child_temp_features1;
        ind=ind+1;
    end
    
    if (sum((child_temp_features1) ~= (temp1_features))~=0) && (sum((child_temp_features1) ~= (temp2_features))~=0)
        child_centers(ind,:)=temp2_centers;
        child_features(ind,:)= child_temp_features2;
        ind=ind+1;
    end
    
    %In the second part of the crossover, we cross only the cluster centers of two
    %parents
    child_temp_centers1=[];
    child_temp_centers2=[];

    for j=1:p
        if mod(j,2) == 1
            child_temp_centers1(j) = temp1_centers(j);
            child_temp_centers2(j) = temp2_centers(j);
        else
            child_temp_centers1(j) = temp2_centers(j);
            child_temp_centers2(j) = temp1_centers(j);
        end
    end
    
    %After the crossover we control the offsprings. If an offspring's
    %center set is same with any of its parents OR if a data point assigned
    %to more than 1 cluster center we do not add that offspring to the
    %population.
    
    if (sum(sort(child_temp_centers1) ~= sort(temp1_centers))~=0) && (sum(sort(child_temp_centers1) ~= sort(temp2_centers))~=0) && (length(unique(child_temp_centers1)) == length(child_temp_centers1))
        child_centers(ind,:)=child_temp_centers1;
        child_features(ind,:)= temp1_features;
        ind=ind+1;
    end
    if (sum(sort(child_temp_centers2) ~= sort(temp1_centers))~=0) && (sum(sort(child_temp_centers2) ~= sort(temp2_centers))~=0) && (length(unique(child_temp_centers2)) == length(child_temp_centers2))
        child_centers(ind,:)=child_temp_centers2;
        child_features(ind,:)= temp2_features;
        ind=ind+1;
    end 
end
end