%% GMRTTOPO
% Download Global Multi-Resolution Topography (GMRT) data using its web
% service.
%
% Syntax
%   [z, lat, lon] = gmrttopo(latlim, lonlim)
%       returns the topography data within the latitude and longitude
%       limits with the default resolution.
%   [z, lat, lon] = gmrttopo(latlim, lonlim, res)
%       returns the topography with the specified resolution.
%   [z, lat, lon] = gmrttopo(latlim, lonlim, res, type)
%       returns the topography with the specified type (whether to use
%       GEBCO data).
%   [z, lat, lon] = gmrttopo(latlim, lonlim, res, type, dir)
%       saves the data to the specified directory, or not if directory is
%       false.
%   [z, lat, lon] = gmrttopo(latlim, lonlim, res, type, dir, fmt)
%       returns the data in the specified format (ndgrid or meshgrid).
%   [z, lat, lon] = gmrttopo(__, Name, Value)
%   [z, lat, lon, hlat, hlon] = gmrttopo(__)
%
% Input arguments
%   latlim/lonlim - latitude/longitude limits in degrees, specified as a
%       two-element vector.
%       The default range is [20, 30]/[115, 125].
%       Format: numeric, [southern_limit, northern_limit]/
%       [western_limit, eastern_limit].
%   res - resolution of the data
%       Resolution in metres or the resolution level.
%       The default resolution is 'default' (i.e. 'low').
%       Format: numeric scalar or string/char, 'default', 'low', 'med',
%       'high', 'max'.
%   type - type of data
%       The default type is the 'topo'.
%       Format: string/char, 'topo', 'topo-mask'.
%   dir - directory to save the data
%       The default directory is the 'TOPODATA' environment or the 'data'
%       subdirectory.
%       Format: string/char, valid directory path, or logical.
%   fmt - format of the output data
%       The default format is 'ndgrid'.
%       Format: string/char, 'ndgrid', 'meshgrid'.
%   Name-Value pair arguments
%   'Visible' - whether to make a plot
%       The default value is true when no output arguments are requested.
%       Format: logical scalar or string/char, 'on', 'off'.
%   'Silence' - whether to suppress the output
%       The default value is false.
%       Format: logical scalar or string/char, 'on', 'off'.
%   'ForceDownload' - whether to force download the data
%       The default value is false.
%       Format: logical scalar or string/char, 'on', 'off'.
%
% Output arguments
%   z - topography data
%       Format: 2D numeric array
%   lat/lon - latitude/longitude data
%       Format: 1D numeric array
%   hlat/hlon - latitude/longitude meshsize
%       Format: numeric scalar
%
% Last modified by
%   2024-10-04, williameclee@gmail.com (@williameclee)

function varargout = gmrttopo(latlim, lonlim, varargin)
    %% Initialisation
    [latlim, lonlim, type, resolution, isMeshgrid, outputFolder, ...
         makePlot, forceNew, beQuiet] = ...
        parseinputs(latlim, lonlim, varargin{:});

    %% Finding if data already exists
    outputPath = outputfilename(latlim, lonlim, resolution, type, ...
        outputFolder, beQuiet);

    downloadFlag = true;

    if exist(outputPath, 'file') && ~forceNew
        load(outputPath, 'z', 'lat', 'lon', 'hlat', 'hlon')

        % Make sure all the variables are loaded
        if exist('z', 'var') && ...
                exist('lat', 'var') && exist('lon', 'var') && ...
                exist('hlat', 'var') && exist('hlon', 'var')
            downloadFlag = false;

            if ~beQuiet
                fprintf('%s loaded %s\n', upper(mfilename), outputPath)
            end

        end

    end

    %% Retriving data
    if downloadFlag
        % Build the URL
        urlBase = 'https://www.gmrt.org/services/GridServer?';
        ulrLatlon = sprintf('north=%f&west=%f&east=%f&south=%f', ...
            latlim(2), lonlim(1), lonlim(2), latlim(1));
        urlType = sprintf('layer=%s', type);

        if isnumeric(resolution)
            urlResolution = sprintf('mresolution=%d', resolution);
        else
            urlResolution = sprintf('resolution=%s', resolution);
        end

        url = [urlBase, strjoin({ulrLatlon, urlType, urlResolution}, '&')];

        % Download the data
        % Make sure the data is saved to a unique filename
        inputFileBase = 'gmrttopo';
        inputFileDate = datetime('now', 'Format', 'yyyyMMddHHmmss');
        inputFileName = [inputFileBase, '-', char(inputFileDate), '.nc'];

        if ~beQuiet
            fprintf( ...
                '%s downloading from gmrt.org. This make take a while...\n', ...
                upper(mfilename))
        end

        websave(inputFileName, url, weboptions("Timeout", 10));

        % Format the data
        z = ncread(inputFileName, 'z');
        z_size = ncread(inputFileName, 'dimension');
        z = fliplr(reshape(z, z_size(:)'));

        lonlim = ncread(inputFileName, 'x_range');
        latlim = ncread(inputFileName, 'y_range');
        lon = linspace(lonlim(1), lonlim(2), z_size(1));
        lat = linspace(latlim(1), latlim(2), z_size(2));
        hlon = diff(lon(1:2));
        hlat = diff(lat(1:2));

        delete(inputFileName)

        if ~(islogical(outputFolder) && ~outputFolder)
            save(outputPath, 'z', 'lat', 'lon', 'hlat', 'hlon')

            if ~beQuiet
                fprintf('%s saved %s\n', upper(mfilename), outputPath)
            end

        end

    end

    % Transpose the data if meshgrid is requested
    if isMeshgrid
        zOutput = z';
    else
        zOutput = z;
    end

    varargout = {zOutput, lat, lon, hlat, hlon};

    %% Plot
    if nargout > 0 || ~makePlot
        return
    end

    figure(1)
    clf

    imagesc(lon, lat, z')

    axis equal
    set(gca, 'YDir', 'normal')
end

%% Subfunctions
function varargout = parseinputs(varargin)
    ip = inputParser;
    addOptional(ip, 'latlim', [20, 30], ...
        @(x) isnumeric(x) && numel(x) == 2);
    addOptional(ip, 'lonlim', [115, 125], ...
        @(x) isnumeric(x) && numel(x) == 2);
    addOptional(ip, 'Resolution', 'default', ...
        @(x) (isnumeric(x) && isscalar(x)) || ...
        ((ischar(x) || isstring(x)) && ...
        ismember(x, {'default', 'low', 'med', 'high', 'max'})));
    addOptional(ip, 'Type', 'topo', @(x) ischar(x) && ...
        ismember(x, {'topo', 'topo-mask'}));
    addOptional(ip, 'OutputFormat', 'ndgrid', ...
        @(x) (ischar(x) || isstring(x)) && ...
        ismember(x, {'ndgrid', 'meshgrid'}));
    addOptional(ip, 'OutputFolder', '', ...
        @(x) ((ischar(x) || isstring(x)) && isfolder(x)) || islogical(x));
    addParameter(ip, 'Visible', true, ...
        @(x) ((islogical(x) || isnumeric(x)) && isscalar(x)) || ...
        ((ischar(x) || isstring(x)) && ismember(x, {'on', 'off'})));
    addParameter(ip, 'Silence', false, ...
        @(x) ((islogical(x) || isnumeric(x)) && isscalar(x)) || ...
        ((ischar(x) || isstring(x)) && ismember(x, {'on', 'off'})));
    addParameter(ip, 'ForceDownload', false, ...
        @(x) ((islogical(x) || isnumeric(x)) && isscalar(x)) || ...
        ((ischar(x) || isstring(x)) && ismember(x, {'on', 'off'})));
    parse(ip, varargin{:});
    latlim = ip.Results.latlim;
    lonlim = ip.Results.lonlim;
    type = ip.Results.Type;
    resolution = ip.Results.Resolution;
    isMeshgrid = strcmpi(ip.Results.OutputFormat, 'meshgrid');
    outputFolder = ip.Results.OutputFolder;
    makePlot = ip.Results.Visible;
    forceNew = ip.Results.ForceDownload;
    beQuiet = ip.Results.Silence;

    if ischar(beQuiet) || isstring(beQuiet)

        switch beQuiet
            case 'on'
                beQuiet = true;
            case 'off'
                beQuiet = false;
        end

    else
        beQuiet = logical(beQuiet);
    end

    if ischar(makePlot) || isstring(makePlot)

        switch makePlot
            case 'on'
                makePlot = true;
            case 'off'
                makePlot = false;
        end

    else
        makePlot = logical(makePlot);
    end

    if ischar(forceNew) || isstring(forceNew)

        switch forceNew
            case 'on'
                forceNew = true;
            case 'off'
                forceNew = false;
        end

    else
        forceNew = logical(forceNew);
    end

    varargout = ...
        {latlim, lonlim, type, resolution, isMeshgrid, outputFolder, ...
         makePlot, forceNew, beQuiet};

end

function outputPath = ...
        outputfilename(latlim, lonlim, resolution, type, outputFolder, beQuiet)

    if sign(latlim(1)) >= 0
        outputLat1 = sprintf('%dn', abs(latlim(1)));
    else
        outputLat1 = sprintf('%ds', abs(latlim(1)));
    end

    if sign(latlim(2)) >= 0
        outputLat2 = sprintf('%dn', abs(latlim(2)));
    else
        outputLat2 = sprintf('%ds', abs(latlim(2)));
    end

    if sign(lonlim(1)) >= 0
        outputLon1 = sprintf('%de', abs(lonlim(1)));
    else
        outputLon1 = sprintf('%dw', abs(lonlim(1)));
    end

    if sign(lonlim(2)) >= 0
        outputLon2 = sprintf('%de', abs(lonlim(2)));
    else
        outputLon2 = sprintf('%dw', abs(lonlim(2)));
    end

    outputLocation = ['-', outputLat1, outputLat2, outputLon1, outputLon2];

    switch type
        case 'topo'
            outputType = '';
        case 'topo-mask'
            outputType = '-mask';
    end

    if isnumeric(resolution)
        outputResolution = sprintf('-R%d', resolution);
    else

        switch resolution
            case {'default', 'low'}
                outputResolution = '-Rl';
            case 'med'
                outputResolution = '-Rm';
            case 'high'
                outputResolution = '-Rh';
            case 'max'
                outputResolution = '-Rf';
            otherwise
                error('Unrecognised resolution')
        end

    end

    outputName = ...
        [upper('gmrttopo'), outputLocation, outputResolution, ...
         outputType, '.mat'];

    if islogical(outputFolder)
        outputPath = outputName;
    else

        if isempty(outputFolder)

            try
                outputFolder = getenv('TOPODATA');
            catch
                outputFolder = 'data';

                if ~exist(outputFolder, 'dir')
                    mkdir(outputFolder)

                    if ~beQuiet
                        fprintf('%s created %s\n', ...
                            upper(mfilename), outputFolder)
                    end

                end

            end

        end

        outputPath = fullfile(outputFolder, outputName);
    end

end
