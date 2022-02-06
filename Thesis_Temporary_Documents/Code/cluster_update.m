function[fitness_value, cluster_set] = cluster_update (data, feature_set,center_set,p,n,d)
% we assign data points to the nearest cluster, and then calculate fitness
% component for each cluster

dd=length(feature_set)/p; % number of clusters

% manhattan distance is utilized for assignment

cluster_distance=[];
for i=1:p  
    center_of_selected_features = feature_set(dd*(i-1)+1:dd*i).*data(center_set(i),:);
    %Here, first ve calculate the distance between i'th cluster center and all
    %data points.
    cluster_distance(:,i) = sum(abs((data - repmat(center_of_selected_features,n,1)).*feature_set(dd*(i-1)+1:dd*i)),2);
end

% we deduce the assignment of data points 
[distance_of_point, cluster_set] = min(cluster_distance,[],2);

for j=1:p
    fitness(j) = sum((distance_of_point.*(cluster_set==j)));
end

%fitness of an offspring equals to sum of sub-fitness values.
fitness_value = sum(fitness);

end