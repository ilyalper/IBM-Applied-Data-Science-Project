function [ind,child_centers,child_features ] = uniform_crossover(ind,p,d,population_size,new_parent_feature_set,new_parent_center_set)

child_features=zeros(1,p*d);
child_centers=zeros(1,p);
for k=1:population_size/2
    temp1_features=new_parent_feature_set(k,:);
    temp2_features=new_parent_feature_set(population_size-k+1,:);
    temp1_centers=new_parent_center_set(k,:);
    temp2_centers=new_parent_center_set(population_size-k+1,:);
    
    %ilk crossover da sadece featureları
    %çaprazlıyoruz
    
    child_temp_features1=[];
    child_temp_features2=[];
    
    feature_pool_1 = [transpose(reshape(temp1_features,d,p))];
    feature_pool_2 = [transpose(reshape(temp2_features,d,p))];
    crossover_mask=randperm(p,round(p/2));
    selection_temp_1=ones(1,p);
    selection_temp_1(crossover_mask)=0;
    
    
    for i=1:p
        if selection_temp_1(i)==1
            child_temp_features1(d*(i-1)+1:d*i)=feature_pool_1(i,:);
            child_temp_features2(d*(i-1)+1:d*i)=feature_pool_2(i,:);
        elseif selection_temp_1(i)==0
            child_temp_features1(d*(i-1)+1:d*i)=feature_pool_2(i,:);
            child_temp_features2(d*(i-1)+1:d*i)=feature_pool_1(i,:);
        end
    end
    
    
    
    if (sum((child_temp_features1) ~= (temp1_features))~=0) && (sum((child_temp_features1) ~= (temp2_features))~=0)
        child_centers(ind,:)=temp1_centers;
        child_features(ind,:)= child_temp_features1;
        ind=ind+1;
    end
    
    if (sum((child_temp_features2) ~= (temp1_features))~=0) && (sum((child_temp_features2) ~= (temp2_features))~=0)
        child_centers(ind,:)=temp2_centers;
        child_features(ind,:)= child_temp_features2;
        ind=ind+1;
    end
    
    
    
    %ikinci crossover da sadece centerları
    %çaprazlıyoruz
    child_temp_centers1=zeros(1,p);
    child_temp_centers2=zeros(1,p);
    
    
    
   
    center_selection_temp_1=ones(1,p);
    center_selection_temp_1(crossover_mask)=0;
    
    
    for i=1:p
        if center_selection_temp_1(i)==1
            child_temp_centers1(i)=temp1_centers(i);
            child_temp_centers2(i)=temp2_centers(i);
        elseif center_selection_temp_1(i)==0
            child_temp_centers1(i)=temp2_centers(i);
            child_temp_centers2(i)=temp1_centers(i);
        end
    end
    
    
    %çaprazlamanın ardından çoçuklardan birinin merkezleri parent'ı ile aynıysa ya da çocuk aynı centerdan birden fazla içeriyorsa bu çocuğu populasyona katmıyoruz.
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