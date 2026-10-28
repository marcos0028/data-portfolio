clear
clc

datos = readtable("datos_depurados.xlsx","TextType","string");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DATOS DEL DISEÑO MUESTRAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

k = norminv(1-(1-0.95)/2);  % intervalo de confianza al 95%

% Estratos: pais + carrera + curso

paish = [
    "España"
    "España"
    "España"
    "España"
    "España"
    "España"
    "Italia"
    "Italia"
    "Italia"
    "Italia"
];

carrerah = [
    "Estadística"
    "Estadística"
    "Ingeniería Informática"
    "Ingeniería Informática"
    "Doble Grado"
    "Doble Grado"
    "Estadística"
    "Estadística"
    "Ingeniería Informática"
    "Ingeniería Informática"
];

cursoh = [
    "Primer curso"
    "Otro curso"
    "Primer curso"
    "Otro curso"
    "Primer curso"
    "Otro curso"
    "Primer curso"
    "Otro curso"
    "Primer curso"
    "Otro curso"
];

% Aquí tienes que meter los tamaños reales de población de cada estrato
Nh = [
    20    % España - Estadística - Primer curso
    69    % España - Estadística - Otro curso
    132   % España - Ingeniería Informática - Primer curso
    353   % España - Ingeniería Informática - Otro curso
    15    % España - Doble Grado - Primer curso
    70    % España - Doble Grado - Otro curso
    143   % Italia - Estadística - Primer curso
    335  % Italia - Estadística - Otro curso
    151   % Italia - Ingeniería Informática - Primer curso
    414   % Italia - Ingeniería Informática - Otro curso
];

N = sum(Nh);
Wh = Nh/N;
L = length(Nh);

% Tamaños muestrales por estrato
nh = zeros(L,1);

for h0 = 1:L

    mh = datos.pais == paish(h0) & ...
         datos.carrera == carrerah(h0) & ...
         datos.curso == cursoh(h0);

    nh(h0) = sum(mh);

end

resumen_estratos = table(paish,carrerah,cursoh,Nh,Wh,nh);
disp("Resumen de los estratos")
disp(resumen_estratos)

%.{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA IA
% En una escala del 0 al 10, cuanto preocupa la IA
% Se estima la media
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp("=========================IA=========================")

y = datos.ia;

estMuh = zeros(L,1);
estvMuh = zeros(L,1);
emMuh = zeros(L,1);
BMuh = zeros(L,1);
icMuh = zeros(L,2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Estimacion por estratos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for h1 = 1:L

    mh = datos.pais == paish(h1) & ...
         datos.carrera == carrerah(h1) & ...
         datos.curso == cursoh(h1);

    ymh = y(mh);

    estMuh(h1) = mean(ymh);

    fh = nh(h1)/Nh(h1);

    estvMuh(h1) = (1-fh)*var(ymh)/nh(h1);
    emMuh(h1) = sqrt(estvMuh(h1));
    BMuh(h1) = k*emMuh(h1);
    icMuh(h1,:) = [estMuh(h1)-BMuh(h1), estMuh(h1)+BMuh(h1)];

end

tabla_ia_estratos = table(paish,carrerah,cursoh,nh,Nh,estMuh,icMuh(:,1),icMuh(:,2), ...
    'VariableNames',{'pais','carrera','curso','nh','Nh','estimacion','LI','LS'});

disp("Tabla ia estratos")
disp(tabla_ia_estratos)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Globalizo por pais + carrera
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grupos_pc = unique(table(paish,carrerah));
Lpc=height(grupos_pc);
nombre_pais_pc = strings(Lpc,1);
nombre_carrera_pc = strings(Lpc,1);
estMupc = zeros(Lpc,1);
estvMupc = zeros(Lpc,1);
emMupc = zeros(Lpc,1);
BMupc = zeros(Lpc,1);
icMupc = zeros(Lpc,2);

for g1 = 1:Lpc

    pos = paish == grupos_pc.paish(g1) & carrerah == grupos_pc.carrerah(g1);

    Nhg = Nh(pos);
    Whg = Nhg/sum(Nhg);

    nombre_pais_pc(g1) = grupos_pc.paish(g1);
    nombre_carrera_pc(g1) = grupos_pc.carrerah(g1);

    estMupc(g1) = sum(Whg.*estMuh(pos));
    estvMupc(g1) = sum((Whg.^2).*estvMuh(pos));

    emMupc(g1) = sqrt(estvMupc(g1));
    BMupc(g1) = k*emMupc(g1);
    icMupc(g1,:) = [estMupc(g1)-BMupc(g1), estMupc(g1)+BMupc(g1)];

end

tabla_ia_pais_carrera = table(nombre_pais_pc,nombre_carrera_pc,estMupc,icMupc(:,1),icMupc(:,2), ...
    'VariableNames',{'pais','carrera','estimacion','LI','LS'});

disp("Tabla ia pais carrera")
disp(tabla_ia_pais_carrera)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Globalizo por pais
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paises = unique(paish);
Lp=length(paises);
estMupais = zeros(Lp,1);
estvMupais = zeros(Lp,1);
emMupais = zeros(Lp,1);
BMupais = zeros(Lp,1);
icMupais = zeros(Lp,2);

for c1 = 1:Lp

    pos = paish == paises(c1);

    Nhc = Nh(pos);
    Whc = Nhc/sum(Nhc);

    estMupais(c1) = sum(Whc.*estMuh(pos));
    estvMupais(c1) = sum((Whc.^2).*estvMuh(pos));

    emMupais(c1) = sqrt(estvMupais(c1));
    BMupais(c1) = k*emMupais(c1);
    icMupais(c1,:) = [estMupais(c1)-BMupais(c1), estMupais(c1)+BMupais(c1)];

end

tabla_ia_pais = table(paises,estMupais,icMupais(:,1),icMupais(:,2), ...
    'VariableNames',{'pais','estimacion','LI','LS'});

disp("Tabla ia pais")
disp(tabla_ia_pais)

writetable(tabla_ia_estratos,"estimaciones_ia_estratos.xlsx")
writetable(tabla_ia_pais_carrera,"estimaciones_ia_pais_carrera.xlsx")
writetable(tabla_ia_pais,"estimaciones_ia_pais.xlsx")

%{


%}
%.{

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA MODALIDAD
% ¿Qué modalidad de trabajo prefieres?
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.modalidad;
%nombre_pregunta = "modalidad";


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA SECTOR
% ¿Qué sector de trabajo prefieres?
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.sector;
%nombre_pregunta = "sector";


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA VIVIR LEJOS
% ¿Estarías dispuesto/a a vivir fuera de tu lugar de origen? 
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.vivir_lejos;
%nombre_pregunta = "vivir_lejos";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA ENCONTRAR EMPLEO
% ¿¿Esperas encontrar empleo en tu sector? 
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.encontrar_empleo;
%nombre_pregunta = "encontrar_empleo";


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA SALARIO
% ¿Qué salario en tu primer empleo esperas? 
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.salario;
%nombre_pregunta = "salario";


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA opornidades
%¿En qué país crees que existen mejores oportunidades laborales?
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta = datos.oportunidades_pais;
%nombre_pregunta = "oportunidades_pais";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA trabajar Es It
%¿Considerarias trabajar en Esp/It?
% Se estiman proporciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pregunta = datos.trabajar_IT_ES;
nombre_pregunta = "trabajar_IT_ES";



opciones = unique(pregunta);
opciones(opciones=="") = [];

RES = table();

for o = 1:length(opciones)

    opcion = opciones(o);

    estPh = zeros(L,1);
    estvPh = zeros(L,1);
    emPh = zeros(L,1);
    BPh = zeros(L,1);
    icPh = zeros(L,2);

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Estimacion por estratos
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for h2 = 1:L

        mh2 = datos.pais == paish(h2) & ...
             datos.carrera == carrerah(h2) & ...
             datos.curso == cursoh(h2);

        ymh2 = pregunta(mh2) == opcion;

        estPh(h2) = mean(ymh2);

        fh2 = nh(h2)/Nh(h2);

        estvPh(h2) = (1-fh2)*estPh(h2)*(1-estPh(h2))/(nh(h2)-1);
        emPh(h2) = sqrt(estvPh(h2));
        BPh(h2) = k*emPh(h2);

        icPh(h2,:) = [estPh(h2)-BPh(h2), estPh(h2)+BPh(h2)];

        if icPh(h2,1) < 0
            icPh(h2,1) = 0;
        end

        if icPh(h2,2) > 1
            icPh(h2,2) = 1;
        end

    end

    tabla_estratos = table(repmat("Estrato",L,1),paish,carrerah,cursoh,repmat(opcion,L,1),nh,Nh,estPh,icPh(:,1),icPh(:,2), ...
        'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

    RES = [RES; tabla_estratos];


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Globalizo por pais + carrera
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    grupos_pc = unique(table(paish,carrerah));

    for g = 1:height(grupos_pc)

        pos = paish == grupos_pc.paish(g) & carrerah == grupos_pc.carrerah(g);

        Nhg = Nh(pos);
        Whg = Nhg/sum(Nhg);

        estPpc = sum(Whg.*estPh(pos));
        estvPpc = sum((Whg.^2).*estvPh(pos));

        emPpc = sqrt(estvPpc);
        BPpc = k*emPpc;

        icPpc = [estPpc-BPpc, estPpc+BPpc];

        if icPpc(1) < 0
            icPpc(1) = 0;
        end

        if icPpc(2) > 1
            icPpc(2) = 1;
        end

        fila1 = table("Global pais-carrera",grupos_pc.paish(g),grupos_pc.carrerah(g),"Todos",opcion,sum(nh(pos)),sum(Nh(pos)),estPpc,icPpc(1),icPpc(2), ...
            'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

        RES = [RES; fila1];

    end


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Globalizo por pais
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    paises = unique(paish);

    for c = 1:length(paises)

        pos = paish == paises(c);

        Nhg = Nh(pos);
        Whg = Nhg/sum(Nhg);

        estPglobal = sum(Whg.*estPh(pos));
        estvPglobal = sum((Whg.^2).*estvPh(pos));

        emPglobal = sqrt(estvPglobal);
        BPglobal = k*emPglobal;

        icPglobal = [estPglobal-BPglobal, estPglobal+BPglobal];

        if icPglobal(1) < 0
            icPglobal(1) = 0;
        end

        if icPglobal(2) > 1
            icPglobal(2) = 1;
        end

        fila2 = table("Global pais",paises(c),"Todas","Todos",opcion,sum(nh(pos)),sum(Nh(pos)),estPglobal,icPglobal(1),icPglobal(2), ...
            'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

        RES = [RES; fila2];

    end

end

%disp(RES)
writetable(RES,"resultados_modalidad.xlsx")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tabla final por pais para una pregunta categorica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

R = RES(RES.nivel == "Global pais",:);

% Pasar a porcentaje
R.estimacion = 100*R.estimacion;
R.LI = 100*R.LI;
R.LS = 100*R.LS;

opciones = unique(R.opcion,'stable');

opcion = strings(length(opciones),1);
estEsp = zeros(length(opciones),1);
LIEsp = zeros(length(opciones),1);
LSEsp = zeros(length(opciones),1);
estIta = zeros(length(opciones),1);
LIIta = zeros(length(opciones),1);
LSIta = zeros(length(opciones),1);

for i = 1:length(opciones)

    opcion(i) = opciones(i);

    filaEsp = R(R.pais == "España" & R.opcion == opciones(i),:);
    filaIta = R(R.pais == "Italia" & R.opcion == opciones(i),:);

    estEsp(i) = filaEsp.estimacion;
    LIEsp(i) = filaEsp.LI;
    LSEsp(i) = filaEsp.LS;

    estIta(i) = filaIta.estimacion;
    LIIta(i) = filaIta.LI;
    LSIta(i) = filaIta.LS;

end

tabla_final = table(opcion,estEsp,LIEsp,LSEsp,estIta,LIIta,LSIta, ...
    'VariableNames',{'opcion','España_estimacion','España_LI','España_LS', ...
    'Italia_estimacion','Italia_LI','Italia_LS'});

disp("Tabla modalidad pais")
disp(tabla_final)

%writetable(tabla_final,"tabla_final_modalidad.xlsx")
%writetable(RES, 'estimaciones_trabajarItEs.xlsx')

%{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Grafico modalidad por pais
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

R = RES(RES.nivel=="Global pais",:);

figure
hold on

for c = 1:length(paises)

    Rc = R(R.pais == paises(c),:);

    errorbar(1:height(Rc),100*Rc.estimacion,100*(Rc.estimacion-Rc.LI),100*(Rc.LS-Rc.estimacion), ...
        'o','LineStyle','none')

end

hold off
set(gca,'XTick',1:length(opciones),'XTickLabel',opciones)
xtickangle(35)
ylabel("Porcentaje estimado")
title("Modalidad de trabajo preferida")
legend(paises)
grid on
%}

%}

%.{


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA ASPECTOS IMPORTANTES
% ¿Qué aspectos valoras más en un futuro trabajo?
% Pregunta de respuesta multiple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta3 = datos.aspectos_imp;
%nombre_pregunta3 = "aspectos_imp";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA ASPECTOS IMPORTANTES
% ¿Qué priblemas ves en tu pais?
% Pregunta de respuesta multiple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%pregunta3 = datos.problemas_tupais;
%nombre_pregunta3 = "problemas_tupais";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREGUNTA ASPECTOS POSITIVOS
% ¿Qué aspectos positivos en tu pais?
% Pregunta de respuesta multiple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pregunta3 = datos.positivos_tupais;
nombre_pregunta3 = "positivos_tupais";

texto = strjoin(pregunta3,",");
opciones = unique(strtrim(split(texto,",")));
opciones(opciones=="") = [];

RES2 = table();

for o = 1:length(opciones)

    opcion3 = opciones(o);

    estPh3 = zeros(L,1);
    estvPh3 = zeros(L,1);
    emPh3 = zeros(L,1);
    BPh3 = zeros(L,1);
    icPh3 = zeros(L,2);

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Estimacion por estratos
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for h3 = 1:L

        mh3 = datos.pais == paish(h3) & ...
              datos.carrera == carrerah(h3) & ...
              datos.curso == cursoh(h3);

        respuestas3 = pregunta3(mh3);

        % Comprobar si cada individuo ha marcado exactamente esa opcion
        ymh3 = false(length(respuestas3),1);

        for j3 = 1:length(respuestas3)

            partes3 = strtrim(split(respuestas3(j3),","));

            if any(partes3 == opcion3)
                ymh3(j3) = true;
            end

        end

        estPh3(h3) = mean(ymh3);

        fh3 = nh(h3)/Nh(h3);

        estvPh3(h3) = (1-fh3)*estPh3(h3)*(1-estPh3(h3))/(nh(h3)-1);
        emPh3(h3) = sqrt(estvPh3(h3));
        BPh3(h3) = k*emPh3(h3);

        icPh3(h3,:) = [estPh3(h3)-BPh3(h3), estPh3(h3)+BPh3(h3)];

        if icPh3(h3,1) < 0
            icPh3(h3,1) = 0;
        end

        if icPh3(h3,2) > 1
            icPh3(h3,2) = 1;
        end

    end

    tabla_estratos = table(repmat("Estrato",L,1),paish,carrerah,cursoh,repmat(opcion3,L,1),nh,Nh,estPh3,icPh3(:,1),icPh3(:,2), ...
        'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

    RES2 = [RES2; tabla_estratos];


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Globalizo por pais + carrera
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    grupos_pc3 = unique(table(paish,carrerah));

    for g3 = 1:height(grupos_pc3)

        pos3 = paish == grupos_pc3.paish(g3) & carrerah == grupos_pc3.carrerah(g3);

        Nhg3 = Nh(pos3);
        Whg3 = Nhg3/sum(Nhg3);

        estPpc3 = sum(Whg3.*estPh3(pos3));
        estvPpc3 = sum((Whg3.^2).*estvPh3(pos3));

        emPpc3 = sqrt(estvPpc3);
        BPpc3 = k*emPpc3;

        icPpc3 = [estPpc3-BPpc3, estPpc3+BPpc3];

        if icPpc3(1) < 0
            icPpc3(1) = 0;
        end

        if icPpc3(2) > 1
            icPpc3(2) = 1;
        end

        fila3 = table("Global pais-carrera",grupos_pc3.paish(g3),grupos_pc3.carrerah(g3),"Todos",opcion3,sum(nh(pos3)),sum(Nh(pos3)),estPpc3,icPpc3(1),icPpc3(2), ...
            'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

        RES2 = [RES2; fila3];

    end


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Globalizo por pais
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    paises = unique(paish);

    for c4 = 1:length(paises)

        pos4 = paish == paises(c4);

        Nhg4 = Nh(pos4);
        Whg4 = Nhg4/sum(Nhg4);

        estPglobal4 = sum(Whg4.*estPh3(pos4));
        estvPglobal4 = sum((Whg4.^2).*estvPh3(pos4));

        emPglobal4 = sqrt(estvPglobal4);
        BPglobal4 = k*emPglobal4;

        icPglobal4 = [estPglobal4-BPglobal4, estPglobal4+BPglobal4];

        if icPglobal4(1) < 0
            icPglobal4(1) = 0;
        end

        if icPglobal4(2) > 1
            icPglobal4(2) = 1;
        end

        fila = table("Global pais",paises(c4),"Todas","Todos",opcion3,sum(nh(pos4)),sum(Nh(pos4)),estPglobal4,icPglobal4(1),icPglobal4(2), ...
            'VariableNames',{'nivel','pais','carrera','curso','opcion','nh','Nh','estimacion','LI','LS'});

        RES2 = [RES2; fila];

    end

end

disp(RES2)
%writetable(RES2,"estimaciones_positivo_pais.xlsx")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TABLA RESUMEN POR PAIS - RESPUESTA MULTIPLE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

R = RES2(RES2.nivel == "Global pais",:);

opciones5 = unique(R.opcion,'stable');

opcion5 = strings(length(opciones5),1);

estEsp5 = zeros(length(opciones5),1);
LIEsp5 = zeros(length(opciones5),1);
LSEsp5 = zeros(length(opciones5),1);

estIta5 = zeros(length(opciones5),1);
LIIta5 = zeros(length(opciones5),1);
LSIta5 = zeros(length(opciones5),1);

dif = zeros(length(opciones5),1);
no_solapan = strings(length(opciones5),1);

for i5 = 1:length(opciones5)

    opcion5(i5) = opciones5(i5);

    filaEsp = R(R.pais == "España" & R.opcion == opciones5(i5),:);
    filaIta = R(R.pais == "Italia" & R.opcion == opciones5(i5),:);

    estEsp5(i5) = 100*filaEsp.estimacion;
    LIEsp5(i5) = 100*filaEsp.LI;
    LSEsp5(i5) = 100*filaEsp.LS;

    estIta5(i5) = 100*filaIta.estimacion;
    LIIta5(i5) = 100*filaIta.LI;
    LSIta5(i5) = 100*filaIta.LS;

    dif(i5) = estEsp5(i5) - estIta5(i5);

    if LSEsp5(i5) < LIIta5(i5) || LSIta5(i5) < LIEsp5(i5)
        no_solapan(i5) = "Sí";
    else
        no_solapan(i5) = "No";
    end

end

tabla_multiple_pais = table(opcion5,estEsp5,LIEsp5,LSEsp5,estIta5,LIIta5,LSIta5,dif,no_solapan, ...
    'VariableNames',{'opcion','España_estimacion','España_LI','España_LS', ...
    'Italia_estimacion','Italia_LI','Italia_LS','dif_España_Italia','no_solapan'});

disp("Tabla aspectos pais")
disp(tabla_multiple_pais)

writetable(tabla_multiple_pais,"tabla_aspectos_imp_pais.xlsx")
%}