function [init_rand_infeasibility_count,parent_features,parent_centers ] = random_initialization(p,d,q,population_size,n,data,init_start,init_end)


%Initial generation of the population
k=init_start;
init_rand_infeasibility_count=0;
while k < init_end+1
    p1= zeros(1,d*p);
    for i = 1:p
        % here, we select the initial random features
        a = randperm(d,q);
        p1(a+((i-1)*d)) = 1;
        
        %here, we select the initial random datapoints
        p2(i) = randperm(n,1);
        
    end

    % First, we control the data points, If a data point is taken as cluster 
    %center for more than 1 cluster we do not consider it and create new
    %random sets
    
    if length(unique(p2)) == length(p2)
        parent_features(k,:) = p1;
        parent_centers(k,:) = p2;
        k=k+1;
    else
        init_rand_infeasibility_count=init_rand_infeasibility_count+1;
    end
end

end
