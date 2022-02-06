function final_feature_set= mutation(feature_set,p)

d = length(feature_set)/p;

i = unidrnd(p);

mutated_feature_set=feature_set(d*(i-1)+1:d*i);

if (sum(mutated_feature_set)==length(mutated_feature_set)) || (sum(mutated_feature_set)==0)
    final_feature_set=feature_set;
    fprintf('please control mutation operator');
else
    [unneces one_indexes] = find(mutated_feature_set==1);
    [unneces zero_indexes] = find(mutated_feature_set==0);
    
    mutated_feature_set(one_indexes(unidrnd(length(one_indexes))))=0;
    mutated_feature_set(zero_indexes(unidrnd(length(zero_indexes))))=1;

    feature_set(d*(i-1)+1:d*i) = mutated_feature_set;
    final_feature_set=feature_set;
    
end

end



