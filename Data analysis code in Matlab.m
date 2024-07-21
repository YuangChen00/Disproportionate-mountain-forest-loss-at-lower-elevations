
%%Load the CSV table generated on Google Earth Engine, which includes count and sum values for potential forest cover and existing forest cover for each 10-meter height elevational band of each mountain
clear all
filenameForest = 'forest_10mband_30.csv';
dataname = 'forest30'
sx='F30_'
forest10mband = readtable(filenameForest,'Delimiter',{'_',':',','},'ReadVariableNames',true,'VariableNamingRule','preserve');
%%Pre-process data
forest10mband(:,6)=[];
PAforest10mband(:,6)=[];
InputT=forest10mband;
toDelete = InputT.count == 0;
InputT(toDelete,:) = [];
InputT.ID=InputT.system+1;
InputT.system= [];InputT.index= [];
InputT = InputT(:,[4 2 3 1]);

%%Divide into 10 elevational bands from the table of 10-meter height elevational bands for each mountain
	n=max(InputT.ID);
	Mountain10=cell(4,n);
	Mountain10{1,1}='ID';
	Mountain10{2,1}='forest';
	Mountain10{3,1}='Potiential forest';
	Mountain10{4,1}='Mountain-wide forest loss rate';
	for i=1:n
		Mountain10{1,i+1}=i;
		T=InputT(InputT.ID==i,:);
		g = Divide(T.count,10);%%Using the 'divide' function listed at the end of this script  
		if isempty(g)
			Mountain10{2,i+1}=nan;
			Mountain10{3,i+1}=nan;
		elseif max(g)<10
			Mountain10{2,i+1}=nan;
			Mountain10{3,i+1}=nan;
		else
			Mountain10{2,i+1}=splitapply(@sum,T.count,g);
			Mountain10{3,i+1}=splitapply(@sum,T.sum,g);
		end
		Mountain10{4,i+1}=1-sum(T.sum)/sum(T.count);
	end 
	clear toDelete n i T g
	
%%%Calculate the remaining forest cover for the lower elevations (as percentages of potential forest cover) for each mountain
%%Lower elevations: the lower three (out of 10) elevational bands, and the lower 500 m above the mountain base
%%Identify mountain base for each mountain
for i=1:max(InputT.ID)
	T=InputT(InputT.ID==i,:);
	MountainBase(i,1)=i;
	if isempty(T.count)||sum(T.count)<16
		MountainBase(i,2)=nan;
	else
		a=1; LowSum=T.count(a);
		while LowSum<16
			a=a+1;
			LowSum=LowSum+T.count(a);
		end
		MountainBase(i,2)=T.dem10(a);
	end
end
clear i a LowSum T

n=max(InputT.ID);
Remain_low_elevation=[];
for i=1:n
	Remain_low_elevation(i,1)=i;
	InputT(find(InputT.dem10<0),:);
	T=InputT(InputT.ID==i,:);
	g = Divide(T.count,10);
	g3=find(g<=3);

	Dem500=find(T.dem10<=(50+MountainBase(i,2)));
		if isempty(g)
			Remain_low_elevation(i,2)=nan;
			Remain_low_elevation(i,3)=nan;
            lowele500(i,1)=nan;
            lowele3(i,1)=nan;
		elseif max(g)<10 | (max(T.dem10)-MountainBase(i,2))<15 | sum(T.count)<300
			Remain_low_elevation(i,2)=nan;
			Remain_low_elevation(i,3)=nan;
            lowele500(i,1)=nan;
            lowele3(i,1)=nan;
		else
			Remain_low_elevation(i,2)=sum(T.sum(g3))./sum(T.count(g3));
			Remain_low_elevation(i,3)=sum(T.sum(Dem500))./sum(T.count(Dem500));
            lowele500(i,1)=(50+MountainBase(i,2))*10;
            lowele3(i,1)=max(T.dem10(g3))*10;
		end
end
clear n i T g g3 Dem500 toDelete
MountainTag_excludeCalculation=isnan(Remain_low_elevation(:,2));


%%%Calculate the remaining forest cover for the lower elevations under protected areas (as percentages of potential forest cover) for each mountain
%%'Lower elevations': the lower three (out of 10) elevational bands, and the lower 500 m above the mountain base
%%load the csv table generated on Google Earth Engine and pre-process data
filenamePAForest = 'PAforest_10mband_30.csv';
PAforest10mband = readtable(filenamePAForest,'Delimiter',{'_',':',','},'ReadVariableNames',true,'VariableNamingRule','preserve');
InputT=PAforest10mband;
toDelete = InputT.count == 0;
InputT(toDelete,:) = [];
InputT.ID=InputT.system+1;
InputT.system= [];InputT.index= [];
InputT = InputT(:,[4 2 3 1]);
clear toDelete n i T g
n=max(InputT.ID);
PAforest_low_elevation=[];
for i=1:n
	PAforest_low_elevation(i,1)=i;
	InputT(find(InputT.dem10<0),:);
	T=InputT(InputT.ID==i,:);
	g = Divide(T.count,10);
	g3=find(g<=3);
	Dem500=find(T.dem10<=(50+MountainBase(i,2)));
		if isempty(g)
			PAforest_low_elevation(i,2)=nan;
			PAforest_low_elevation(i,3)=nan;
            lowele500(i,2)=nan;
            lowele3(i,2)=nan;
		elseif max(g)<10  | (max(T.dem10)-MountainBase(i,2))<15 | sum(T.count)<300
			PAforest_low_elevation(i,2)=nan;
			PAforest_low_elevation(i,3)=nan;
            lowele500(i,2)=nan;
            lowele3(i,2)=nan;
		else
			PAforest_low_elevation(i,2)=sum(T.sum(g3))./sum(T.count(g3));
			PAforest_low_elevation(i,3)=sum(T.sum(Dem500))./sum(T.count(Dem500));
            lowele500(i,2)=(50+MountainBase(i,2))*10;
            lowele3(i,2)=max(T.dem10(g3))*10;
		end
end
clear n i T g g3 Dem500 toDelete    
    

%%%Calculate the FLEC and FLER scores for each mountain
n=10;%the number of elevatioanl bands
	for i=2:max(size(Mountain10))
		if  MountainTag_excludeCalculation(i-1)
			FLER(i-1,1)=nan;
			FLEC(i-1,1)=nan;
            Forest_loss_MWide(i-1,1)=nan;
		    PotentialForest(i-1,1)=nan;
		else
			Forest_loss_rate=1-Mountain10{3,i}./Mountain10{2,i};
			b=Forest_loss_rate;
			b(:,2)=1:n;
			b=sortrows(b);b(:,3)=[1:n];
			FLER(i-1,1)=(sum(b(:,2)'.*b(:,3)')-220)/(385-220);
			b=Forest_loss_rate;
			b(:,2)=1:n;
			b(:,3)=b(:,1)/sum(b(:,1));
			FLEC(i-1,1)=(sum(b(:,2)'.*b(:,3)')-1)/(10-1);
            Forest_loss_MWide(i-1,1)=Mountain10{4,i};
            PotentialForest(i-1,1)=sum(Mountain10{2,i});
        end
		ID(i-1,1)=Mountain10{1,i};
    end
    FLER(Forest_loss_MWide==0|Forest_loss_MWide==1,1)=0.5;
    FLEC(Forest_loss_MWide==0|Forest_loss_MWide==1,1)=0.5;
    Mountain_Index = table(ID,FLER,FLEC,Forest_loss_MWide,PotentialForest);
	clear ID FLER FLEC Forest_loss_MWide b n i PotentialForest


%%%Generate simulations for FLEC and FLER indices on a hypothetical mountain under a null scenario of no elevational patterns in forest loss
%Total pixel number of mountain= M (10000); Number of elevation bands = n (10); 
%Pixel number of one band = e (1000); 
%Mountain-wide forest loss rate = D(0:0.01:1); Forest loss area= M*D; 
%Number of simulation times=st(1000);Sinmulation index = FLER FLEC; interval = probability
M=100000;
n=10;
D=[0.0002 0.01:0.01:0.99 0.9998];
e=M/n;
st=10000;
interval=[0.025 0.05:0.05:0.95 0.975]

for d=1:max(size(D))
	DA=M*D(d);
	[x,v] = randfixedsum(n,st,DA,0,e);
	DP=x/e;
	DAP=x/DA;
			for i=1:st
				b=DP(:,i);
				b(:,2)=1:n;
				b=sortrows(b);b(:,3)=[1:n];
				FLER(i,1)=(sum(b(:,2)'.*b(:,3)')-220)/(385-220);
				b=DAP(:,i);b(:,2)=1:n;
				FLEC(i,1)=(sum(b(:,2)'.*b(:,1)')-1)/(10-1);
			end
			clear i a b
	FLER=sortrows(FLER);FLEC=sortrows(FLEC);
	for j=1:max(size(interval))
		FLER_interval(j,d)=FLER(int32(interval(j)*st-1));
		FLEC_interval(j,d)=FLEC(int32(interval(j)*st-1));
	end
end

%%%Gauge the statistical meaning (including statistical significance) of the FLEC and FLER values
standard=roundn(D,-2);
prob=[0 interval*100];
Table=Mountain_Index;
for i=1:max(size(Table))
	if isnan(Table.FLER(i))
		Mountain_FLER_prob(i,1)=nan;
		Mountain_FLEC_prob(i,1)=nan;
    else if Table.Forest_loss_MWide(i)==0|Table.Forest_loss_MWide(i)==1
        Mountain_FLER_prob(i,1)=50;
		Mountain_FLEC_prob(i,1)=50;  
        else
		tag=find(standard==roundn(Table.Forest_loss_MWide(i),-2));
		t = discretize(Table.FLER(i),[-Inf;FLER_interval(:,tag);Inf]);
		Mountain_FLER_prob(i,1)= prob(t); 
		t = discretize(Table.FLEC(i),[-Inf;FLEC_interval(:,tag);Inf]);
		Mountain_FLEC_prob(i,1)= prob(t); 
        end
	end
end    
Mountain_Index.FLER_prob=Mountain_FLER_prob;
Mountain_Index.FLEC_prob=Mountain_FLEC_prob;
Mountain_Index.RFlow3=Remain_low_elevation(:,2)*100;
Mountain_Index.RFlow500=Remain_low_elevation(:,3)*100;
Mountain_Index.PAlow3=PAforest_low_elevation(:,2)*100;
Mountain_Index.PAlow500=PAforest_low_elevation(:,3)*100;
if max(size(Mountain_Index))<805
    Mountain_Index(805,:)={NaN};
    Mountain_Index(805,1)={805};
end

%%%Calcualte the mountain-wide mean values of MAT and MAP under remaining and potential forest cover
%%load the csv table generated on Google Earth Engine and pre-process data
filenameclim =['Q:\1 GIS\json\Adjust DEM 20240423\GEE table\','mountain_clim_F30']
mountainclim = readtable(filenameclim ,'Delimiter',{'_',':',','},'ReadVariableNames',true,'VariableNamingRule','preserve');
mountainclim(:,6)=[];
EveryMountainClim=mountainclim;
EveryMountainClim.zone_index=EveryMountainClim.system+1;
EveryMountainClim.system= [];EveryMountainClim.index= [];
EveryMountainClim = EveryMountainClim(:,[4 2 3 1]);
toDelete = EveryMountainClim.count == 0;
EveryMountainClim(toDelete,:) = [];
clear toDelete
EveryMountainClim.P=floor(EveryMountainClim.code/100)*100;
EveryMountainClim.T=Temprature_code_GEE(EveryMountainClim.code);%%Function 'Temprature_code_GEE' listed at the end of the script
EveryMountainClim.NotNullID=EveryMountainClim.zone_index;
EveryMountainClim.zone_index= [];
EveryMountainClim.code= [];

for i=1:max(EveryMountainClim.NotNullID)
	clim=EveryMountainClim(EveryMountainClim.NotNullID==i,:);
	RemainMeanT(i,1)=sum(clim.sum.*clim.T)/sum(clim.sum);
	RemainMeanP(i,1)=sum(clim.sum.*clim.P)/sum(clim.sum);
	BastinMeanT(i,1)=sum(clim.count.*clim.T)/sum(clim.count);
	BastinMeanP(i,1)=sum(clim.count.*clim.P)/sum(clim.count);
end
ID=Mountain_Index.ID(~isnan(Mountain_Index.FLER));
MountainClimCenter1=table(ID,RemainMeanT,BastinMeanT,RemainMeanP,BastinMeanP);
clear i ID NativeMeanT BastinMeanT NativeMeanP BastinMeanP clim 


%%%Calcualte the precentage of remaining forest cover across the world's mountains at the lower elevations under the each combination of MAT and MAP bins
%%Combinations of MAT and MAP bins:1 ¡ãC and 100 mm y-1
%%load the csv table generated on Google Earth Engine and pre-process data
filenameclim = ['Q:\1 GIS\json\Adjust DEM 20240423\GEE table\','mountain_clim_F30_low3',Dataname{1,i},'.csv'];
mountainclim = readtable(filenameclim ,'Delimiter',{'_',':',','},'ReadVariableNames',true,'VariableNamingRule','preserve');
mountainclim(:,6)=[];
forestzoneclimprocess=mountainclim;
forestzoneclimprocess.zone_index=forestzoneclimprocess.system+1;
forestzoneclimprocess.system= [];forestzoneclimprocess.index= [];
forestzoneclimprocess = forestzoneclimprocess(:,[4 2 3 1]);
toDelete = forestzoneclimprocess.count == 0;
forestzoneclimprocess(toDelete,:) = [];
clear toDelete
forestzoneclimprocess.P=floor(forestzoneclimprocess.code/100)*100;
forestzoneclimprocess.T=Temprature_code_GEE(forestzoneclimprocess.code);%%Function 'Temprature_code_GEE' listed at the end of the script
Zoneclimprocess=forestzoneclimprocess;
Zoneclimprocess.code=[];
Zoneclimprocess.zone_index=[];
Lia = Zoneclimprocess;
[G,P,T] = findgroups(Lia.P,Lia.T);
Remain=splitapply(@sum,Lia.sum,G);
bastin=splitapply(@sum,Lia.count,G);
AllZoneclim=table(Remain,bastin,P,T);
AllZoneclim.remaining=AllZoneclim.remain./AllZoneclim.bastin*100;
clear remain bastin P T Lia G

%%%Calculate the mean values of each factor(HFP,MAP,water deficit,slope) for each elevational band and mountain-wide values of each factor for each mountain
%%load the csv table generated on Google earth engine and pre-process data
filenameForest = 'HFP_10mband_30.csv';%%'MAP_10mband_30.csv','WaterDeficit_10mband_30.csv','Slope_10mband_30.csv'
forest10mband = readtable(filenameForest,'Delimiter',{'_',':',','},'ReadVariableNames',true,'VariableNamingRule','preserve');
forest10mband(:,6)=[];
InputT=forest10mband;
toDelete = InputT.count == 0;
InputT(toDelete,:) = [];
InputT.ID=InputT.system+1;
InputT.system= [];InputT.index= [];
InputT = InputT(:,[4 2 3 1]);
n=max(InputT.ID);
%%Divide into 10 elevational bands from the table of 10-meter height elevational bands for each mountain
Mountain10=cell(4,n);
Mountain10{1,1}='ID';
Mountain10{2,1}='Sum_Variable';
Mountain10{3,1}='Potiential forest';
Mountain10{4,1}='Mean Value for 10 ele-bands';
Mountain10{5,1}='Mountain-wide Value';
	for i=1:n
		Mountain10{1,i+1}=i;
		T=InputT(InputT.ID==i,:);
		g = Divide(T.count,10);
		if isempty(g)
			Mountain10{2,i+1}=nan;
			Mountain10{3,i+1}=nan;
		elseif max(g)<10
			Mountain10{2,i+1}=nan;
			Mountain10{3,i+1}=nan;
		else
			Mountain10{2,i+1}=splitapply(@sum,T.sum,g);
			Mountain10{3,i+1}=splitapply(@sum,T.count,g);
		end
		Mountain10{4,i+1}=Mountain10{2,i+1}./Mountain10{3,i+1};
        Mountain10{5,i+1}=sum(Mountain10{2,i+1})/sum(Mountain10{3,i+1});
	end 
	clear toDelete n i T g
Mountain10_Forest=Mountain10;
Mountain10_HFP=Mountain10;
Mountain10_MAP=Mountain10;
Mountain10_WaterDeficit=Mountain10;
Mountain10_Slope=Mountain10;
Mountain_ID=[];Elevation=[];HFP=[];MAP=[];WaterDeficit=[];Forest_loss=[];Slope=[];Elevation=[];HFP_MWide=[];MAP_MWide=[];Slope_MWide=[];
WaterDeficit_MWide=[];Forest_loss_MWide=[];
for i=1:805
    if isnan(Mountain_Index.FLER(i))
        
    else
    Elevation=[Elevation;[1:10]'] ;   
    Mountain_ID=[Mountain_ID;ones(10,1)*i];
    HFP=[HFP;Mountain10_HFP{4,i+1}];
    HFP_MWide=[HFP_MWide;ones(10,1)*Mountain10_HFP{5,i+1}];
    MAP=[MAP;Mountain10_MAP{4,i+1}];
    MAP_MWide=[MAP_MWide;ones(10,1)*Mountain10_MAP{5,i+1}];
    WaterDeficit=[WaterDeficit;Mountain10_WaterDeficit{4,i+1}];
    WaterDeficit_MWide=[WaterDeficit_MWide;ones(10,1)*Mountain10_WaterDeficit{5,i+1}];
    Forest_loss=[Forest_loss;1-Mountain10_Forest{4,i+1}];
    Forest_loss_MWide=[Forest_loss_MWide;ones(10,1)*(1-Mountain10_Forest{5,i+1})];
    Slope=[Slope;Mountain10_Slope{4,i+1}];
    Slope_MWide=[Slope_MWide;ones(10,1)*Mountain10_Slope{5,i+1}];
    end
end
ForestLoss_driver_Data=table(Mountain_ID,Elevation,Forest_loss,HFP,Slope,MAP,WaterDeficit,Forest_loss_MWide,HFP_MWide,Slope_MWide,MAP_MWide,WaterDeficit_MWide);

%Function 'divide' used above
function g = Divide(a,n)
% Idea: Divide the array 'a' into 'n' segments such that each segment's sum is as close as possible to the value of evenly dividing 'a' into 'n' parts.
Average=round(sum(a)/n);
	list=a;
	ID=0;IDb=1;
	g=[];
	if max(size(a))<=n 
		g=[];
	else
		for j=1:n 
			flag=0; c=0; Abs_A=0; Abs_B=1; Sum=list(1); 
				while Abs_A<Abs_B 
						if j==n || c+1>=max(size(list))
							% Flag (1):If already dividing into the last segment, remaining data is assigned to the last segment
							flag=1;
							c=size(list);
							ID=size(a);
							break
						end
					c=c+1;
					ID=ID+1;
					Abs_B=abs(Average-(Sum));
					Abs_A=abs(Average-(Sum+list(c+1)));
					Sum=Sum+list(c+1);
				end
			g(IDb:ID)=j;
			list(1:c)=[];
			IDb=IDb+c;
			ID=IDb;
				if flag==1 
					break
				end
		end
	end
	g=g';
    
end

%Function 'Temprature_code_GEE' used above
function T = Temprature_code_GEE(a)
a=num2str(a);
for id=1:length(a)
	num=a(id,:);
	T(id,:)=num(end-1:end);
end
    T=str2num(T);
    T=T-50;
end