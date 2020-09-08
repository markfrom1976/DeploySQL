USE [TEAMS]
-- Config.
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'b__DisplayPortalLegTotalComplianceGraph' AND OBJECT_ID = OBJECT_ID('Config'))
BEGIN
    ALTER TABLE Config
    ADD [b__DisplayPortalLegTotalComplianceGraph] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'b__DisplayPortalLegRiskItemsGraph' AND OBJECT_ID = OBJECT_ID('Config'))
BEGIN
    ALTER TABLE Config
    ADD [b__DisplayPortalLegRiskItemsGraph] BIT NOT NULL DEFAULT 0
END

-- User.
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'TotalComplianceChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [TotalComplianceChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'LegTotalComplianceChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [LegTotalComplianceChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'SitesSurveyedChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [SitesSurveyedChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'RiskItemsChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [RiskItemsChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'LegRiskItemsChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [LegRiskItemsChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'RecommendedActionItemsChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [RecommendedActionItemsChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'TargetTimescalesChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [TargetTimescalesChart] BIT NOT NULL DEFAULT 0
END
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'TrainingComplianceChart' AND OBJECT_ID = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD [TrainingComplianceChart] BIT NOT NULL DEFAULT 0
END
GO

ALTER PROCEDURE [dbo].[GetAppointments]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @StartDate DATETIME = NULL,
    @EndDate DATETIME = NULL,
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @EmployeeID INT = NULL
/**********************************************************************
** Overview: Get a filtered collection of Appointments for the Portal.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @EndDate = ISNULL(@EndDate, @StartDate),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @EmployeeID = NULLIF(@EmployeeID, 0)

    -- Cast DATETIME to DATE. Set the EndDate to midnight.
    SELECT
        @StartDate = CAST(@StartDate AS DATE),
        @EndDate = CAST(CAST(CAST(@EndDate AS DATE) AS VARCHAR(10)) + ' 23:59:59.997' AS DATETIME)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @SiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@SiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END


    -- Start the main SELECT.
    SELECT
        a.AppointmentID,
        a.AppointmentTypeID,
        a.DateConfirmed,
        a.ClientID,
        a.ProjectID,
        a.SiteID,
        a.StartTime,
        a.EndTime,
        a.DateCreated,
        CAST(a.Notes AS VARCHAR(MAX)) [Notes],
        si.Address,
        j.JobNo,
        CASE WHEN j.Approved IS NOT NULL THEN j.SiteID ELSE NULL END [JobSiteID],
        COALESCE(sut.Description, lt.Description, at.AppointmentType) [AppointmentType],
        lt.LegionellaAssetCategoryID,
        CASE WHEN j.Approved IS NOT NULL THEN la.LegionellaAssetID ELSE NULL END [LegionellaAssetID]
    FROM
        Appointment a WITH (NOLOCK)
        INNER JOIN AppointmentType at WITH (NOLOCK) ON a.AppointmentTypeID = at.AppointmentTypeID
        LEFT JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
        LEFT JOIN SurveyType sut WITH (NOLOCK) ON asu.SurveyTypeID = sut.SurveyTypeID
        LEFT JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
        LEFT JOIN LegionellaType lt WITH (NOLOCK) ON al.LegionellaTypeID = lt.LegionellaTypeID
        INNER JOIN Client c WITH (NOLOCK) ON a.ClientID = c.ClientID
        INNER JOIN @ClientIdData cid ON c.ClientID = cid.ClientID
        LEFT JOIN AppointmentEmployee ae WITH (NOLOCK) ON a.AppointmentID = ae.AppointmentID
        LEFT JOIN Employee e WITH (NOLOCK) ON ae.EmployeeID = e.EmployeeID
        LEFT JOIN Project p WITH (NOLOCK) ON a.ProjectID = p.ProjectID
        LEFT JOIN Site si WITH (NOLOCK) ON a.SiteID = si.SiteID
        LEFT JOIN Quote q WITH (NOLOCK) ON a.QuoteID = q.QuoteID
        LEFT JOIN Job j WITH (NOLOCK) ON q.JobID = j.JobID
        OUTER APPLY
        (
            SELECT TOP 1
                la.LegionellaAssetID
            FROM
                JobEmployee je WITH (NOLOCK)
                INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID
                INNER JOIN LegionellaAsset la WITH (NOLOCK) ON l.LegionellaID = la.LegionellaID AND l.DateApproved IS NOT NULL
            WHERE
                je.JobID = j.JobID
                    AND
                la.LegionellaAssetCategoryID = lt.LegionellaAssetCategoryID
                    AND
                la.Deleted IS NULL
            ORDER BY
                l.LegionellaStart,
                la.SortOrder
        ) la
    WHERE
        si.Deleted IS NULL
            AND
        a.DateDeclined IS NULL
            AND
        a.ProjectAppointmentID IS NULL
            AND
        (
            DATEADD(dd, 0, DATEDIFF(dd, 0, a.StartTime)) BETWEEN @StartDate AND @EndDate
                OR
            DATEADD(dd, 0, DATEDIFF(dd, 0, a.EndTime)) BETWEEN @StartDate AND @EndDate
        )
            AND
        (@ProjectGroupID IS NULL OR p.ProjectGroupID = @ProjectGroupID)
            AND
        (@ProjectID IS NULL OR p.ProjectID = @ProjectID)
            AND
        (
            @SiteIDs IS NULL
                OR
            (
                @SiteIDs IS NOT NULL
                    AND
                si.SiteID IN (SELECT SiteID FROM @SiteIdData)
            )
        )
            AND
        (
            @ClientOrderNo IS NULL
                OR
            (
                @ClientOrderNo IS NOT NULL
                    AND
                (a.ClientOrderNo LIKE '%' + @ClientOrderNo + '%' OR j.ClientOrderNo LIKE '%' + @ClientOrderNo + '%')
            )
        )
            AND
        (@UPRN IS NULL OR (@UPRN IS NOT NULL AND si.UPRN = @UPRN))
            AND
        (
            @AddressSearchString IS NULL
                OR
            (
                @AddressSearchString IS NOT NULL
                    AND
                (
                    si.Address LIKE '%' + @AddressSearchString + '%'
                        OR
                    si.PostCode LIKE '%' + @AddressSearchString + '%'
                        OR
                    si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @AddressSearchString + '%'
                        OR
                    si.UPRN = @AddressSearchString
                        OR
                    j.ClientOrderNo = @AddressSearchString
                )
            )
        )
            AND
        (@EmployeeID IS NULL OR ae.EmployeeID = @EmployeeID)
    GROUP BY
        a.AppointmentID,
        a.AppointmentTypeID,
        a.DateConfirmed,
        a.ClientID,
        a.ProjectID,
        a.SiteID,
        a.StartTime,
        a.EndTime,
        a.DateCreated,
        CAST(a.Notes AS VARCHAR(MAX)),
        si.Address,
        j.JobNo,
        j.Approved,
        j.SiteID,
        at.AppointmentType,
        sut.Description,
        lt.Description,
        lt.LegionellaAssetCategoryID,
        la.LegionellaAssetID


    SET NOCOUNT OFF;
END
GO
ALTER VIEW [dbo].[PortalActivity]
AS

SELECT DISTINCT
    pat.PortalAuditTrailID,
    pu.Company,
    pu.PortalUserID [UserId],
    pu.FullName [User],
    pat.Message,
    pat.DataIPAddress,
    pat.DateCreated [DateAccessed],
    COALESCE(j.JobNo, jpf.JobNo) [JobNo],
    cpu.ClientId,
    COALESCE(pj.ProjectID, pssid.ProjectID, ppf.ProjectID) [ProjectId],
    COALESCE(sij.SiteID, sisid.SiteID, sipf.SiteID) [SiteId],
    CASE
        WHEN sij.SiteID IS NOT NULL THEN sij.Address + ', ' + sij.Postcode
        WHEN sisid.SiteID IS NOT NULL THEN sisid.Address + ', ' + sisid.Postcode
        WHEN sipf.SiteID IS NOT NULL THEN sipf.Address + ', ' + sipf.Postcode
    END [Address],
    COALESCE(sij.Postcode, sisid.Postcode, sipf.Postcode) [Postcode],
    COALESCE(sij.UPRN, sisid.UPRN, sipf.UPRN) [UPRN]
FROM
    PortalAuditTrail pat WITH (NOLOCK)
    LEFT JOIN PortalUser pu WITH (NOLOCK) ON pat.PortalUserID = pu.PortalUserID
    LEFT JOIN ClientPortalUser cpu WITH (NOLOCK) ON pu.PortalUserID = cpu.PortalUserID

    -- Job joins.
    LEFT JOIN Job j WITH (NOLOCK) ON pat.DataID = j.JobID AND pat.DataTable = 'Job'
    LEFT JOIN Site sij WITH (NOLOCK) ON j.SiteID = sij.SiteID
    LEFT JOIN ProjectSite psj WITH (NOLOCK) ON sij.SiteID = psj.SiteID
    LEFT JOIN Project pj WITH (NOLOCK) ON psj.ProjectID = pj.ProjectID AND pj.ClientID = pu.ClientID

    -- Site Document joins.
    LEFT JOIN SiteDocument sid WITH (NOLOCK) ON pat.DataID = sid.SiteDocumentID AND pat.DataTable = 'SiteDocument'
    LEFT JOIN Site sisid WITH (NOLOCK) ON sid.SiteID = sisid.SiteID
    LEFT JOIN ProjectSite pssid WITH (NOLOCK) ON sisid.SiteID = pssid.SiteID
    LEFT JOIN Project psid WITH (NOLOCK) ON pssid.ProjectID = psid.ProjectID AND psid.ClientID = pu.ClientID

    -- PDF joins.
    LEFT JOIN PDF pf WITH (NOLOCK) ON pat.DataID = pf.PdfID AND pat.DataTable = 'PDF'
    LEFT JOIN Job jpf WITH (NOLOCK) ON pf.JobID = jpf.JobID
    LEFT JOIN Site sipf WITH (NOLOCK) ON jpf.SiteID = sipf.SiteID
    LEFT JOIN ProjectSite pspf WITH (NOLOCK) ON sipf.SiteID = pspf.SiteID
    LEFT JOIN Project ppf WITH (NOLOCK) ON pspf.ProjectID = ppf.ProjectID AND ppf.ClientID = pu.ClientID

GO
ALTER PROCEDURE [dbo].[GetPortalActivity]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @ActivityDateFrom DATETIME = NULL,
    @ActivityDateTo DATETIME = NULL,
    @JobNo INT = NULL,
    @FilterPortalUserID INT = NULL,
    @FilterCompany VARCHAR(100) = NULL
/**********************************************************************
** Overview: Get a filtered collection of Portal Activities.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @ActivityDateFrom = ISNULL(@ActivityDateFrom, '2008-01-01'),
        @ActivityDateTo = ISNULL(@ActivityDateTo, GETDATE()),
        @JobNo = NULLIF(@JobNo, 0),
        @FilterPortalUserID = NULLIF(@FilterPortalUserID, 0),
        @FilterCompany = NULLIF(LTRIM(RTRIM(@FilterCompany)), '')

    -- Cast DATETIME to DATE. Add one day to the DateTo - this will set @ActivityDateTo as the next day but it will be at midnight.
    SET @ActivityDateFrom  = CAST(@ActivityDateFrom AS DATE)
    SET @ActivityDateTo = DATEADD(d, 1, CAST(@ActivityDateTo AS DATE))

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocClientOrderNo VARCHAR(50) = @ClientOrderNo,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocActivityDateFrom DATETIME = @ActivityDateFrom,
        @LocActivityDateTo DATETIME = @ActivityDateTo,
        @LocJobNo INT = @JobNo,
        @LocFilterPortalUserID INT = @FilterPortalUserID,
        @LocFilterCompany VARCHAR(100) = @FilterCompany

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    CREATE TABLE #SiteIdData (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO #SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(siid.s)) [SiteID]
        FROM
            dbo.SplitString(@LocSiteIDs, ',') siid
            INNER JOIN Site si WITH (NOLOCK) ON LTRIM(RTRIM(siid.s)) = si.SiteID
        WHERE
            NULLIF(LTRIM(RTRIM(siid.s)), '') IS NOT NULL
                AND
            si.Deleted IS NULL
                AND
            CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                THEN 1
                ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                THEN 1
                ELSE
                    CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString
                        THEN 1
                        ELSE 0
                    END
            END = 1
        GROUP BY siid.s
    END
    ELSE
    BEGIN -- For this SPROC, we ALWAYS have to get the SiteIDs that the user has access to.
        IF @LocProjectGroupID IS NOT NULL OR @LocProjectID IS NOT NULL
        BEGIN
            INSERT INTO #SiteIdData (SiteID)
            SELECT si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
                INNER JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
                INNER JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
            WHERE
                si.Deleted IS NULL
                    AND
                p.Deleted IS NULL
                    AND
                CASE WHEN @LocProjectGroupID IS NOT NULL
                    THEN CASE WHEN p.ProjectGroupID = @ProjectGroupID THEN 1 ELSE 0 END
                    ELSE CASE WHEN p.ProjectID = @ProjectID THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                    THEN 1
                    ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                    THEN 1
                    ELSE
                        CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString
                            THEN 1
                            ELSE 0
                        END
                END = 1
            GROUP BY si.SiteID
        END
        ELSE
        BEGIN
            INSERT INTO #SiteIdData (SiteID)
            SELECT si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            WHERE
                si.Deleted IS NULL
                    AND
                CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                    THEN 1
                    ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                    THEN 1
                    ELSE
                        CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString
                            THEN 1
                            ELSE 0
                        END
                END = 1
            GROUP BY si.SiteID
        END
    END

    -- Get all Companies up front to reduce table scans on the PortalUser table and only get the ones needed. We use this for filtering purposes.
    DECLARE @PortalUserCompanies TABLE (Company VARCHAR(100) NOT NULL)

    INSERT INTO @PortalUserCompanies (Company)
    SELECT pu.Company
    FROM
        #ClientIdData c
        INNER JOIN ClientPortalUser cpu WITH (NOLOCK) ON c.ClientID = cpu.ClientID
        INNER JOIN PortalUser pu WITH (NOLOCK) ON cpu.PortalUserID = pu.PortalUserID
    WHERE
        pu.Deleted IS NULL
            AND
        NULLIF(LTRIM(RTRIM(pu.Company)), '') IS NOT NULL
    GROUP BY
        pu.Company
    ORDER BY
        pu.Company

    -- Get all of the Portal Audit data up front. Minimum filters used, as we use more below.
    DECLARE @PortalAuditTrailData TABLE (PortalAuditTrailID INT PRIMARY KEY, UserID INT NOT NULL, [User] VARCHAR(100) NOT NULL, Message VARCHAR(500) NOT NULL, DataTable VARCHAR(50) NOT NULL, DataID INT NOT NULL, DataIPAddress VARCHAR(50) NOT NULL, DateCreated DATETIME NOT NULL, Company VARCHAR(100) NOT NULL)

    INSERT INTO @PortalAuditTrailData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company)
    SELECT
        pat.PortalAuditTrailID,
        put.PortalUserID [UserID],
        put.FullName [User],
        pat.Message,
        pat.DataTable,
        pat.DataID,
        pat.DataIPAddress,
        pat.DateCreated,
        put.Company
    FROM
        PortalAuditTrail pat WITH (NOLOCK)
        INNER JOIN PortalUser put WITH (NOLOCK) ON pat.PortalUserID = put.PortalUserID
        INNER JOIN @PortalUserCompanies puc ON put.Company = puc.Company
    WHERE -- Some of these use a CASE for short circuiting purposes.
        pat.PortalUserID IS NOT NULL
            AND
        (pat.DateCreated >= @LocActivityDateFrom AND pat.DateCreated < @LocActivityDateTo)
            AND
        CASE WHEN @LocFilterPortalUserID IS NULL -- Portal User Filter.
            THEN 1
            ELSE CASE WHEN pat.PortalUserID = @LocFilterPortalUserID THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocFilterCompany IS NULL -- Company Filter.
            THEN 1
            ELSE CASE WHEN ISNULL(put.Company, '') = @LocFilterCompany THEN 1 ELSE 0 END
        END = 1

    -- Get the main data we are exporting into another table variable. For the data in this, we filter it down to the Clients and Sites we have access to.
    DECLARE @MainData TABLE (PortalAuditTrailID INT PRIMARY KEY, UserID INT NOT NULL, [User] VARCHAR(100) NOT NULL, Message VARCHAR(500) NOT NULL, DataTable VARCHAR(50) NOT NULL, DataID INT NOT NULL, DataIPAddress VARCHAR(50) NOT NULL, DateCreated DATETIME NOT NULL, Company VARCHAR(100) NOT NULL, JobNo INT, Address VARCHAR(200), Postcode VARCHAR(10), UPRN VARCHAR(50), Project VARCHAR(MAX))

    /* 1. Tables that need validation based upon the Portal User. For these, we just have to join to the current User's Clients to check for access. */
    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable IN ('PortalUser', 'PortalUserTraining'))
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
		
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            NULL [JobNo],
            NULL [Address],
            NULL [Postcode],
            NULL [UPRN],
			NULL [Project]
        FROM
            @PortalAuditTrailData pat
            INNER JOIN ClientPortalUser cpu WITH (NOLOCK) ON pat.UserID = cpu.PortalUserId
            INNER JOIN #ClientIdData c ON cpu.ClientId = c.ClientID
        WHERE
            pat.DataTable IN ('PortalUser', 'PortalUserTraining')
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company
    END

    /* 2. Tables that need validation based upon the Job. For these, we just have to join to the current User's Clients and Sites from the Job to check for access. */
    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Job')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Job j WITH (NOLOCK) ON pat.DataID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Job'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'PDF')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN PDF pf WITH (NOLOCK) ON pat.DataID = pf.PDFId
            CROSS APPLY
            (
                SELECT 1 [RowType], _j.JobID, _j.JobNo, NULL [QuoteID], NULL [InvoiceID], _j.ClientID, _j.SiteID, _j.ProjectID
                FROM Job _j WITH (NOLOCK)
                WHERE
                    _j.JobID = pf.JobID
                        AND
                    CASE WHEN @LocJobNo IS NULL -- JobNo Filter. NOTE: Done twice in this part of SQL to improve speed!
                        THEN 1
                        ELSE CASE WHEN _j.JobNo = @LocJobNo THEN 1 ELSE 0 END
                    END = 1

                UNION ALL

                SELECT 2 [RowType], NULL [JobID], NULL [JobNo], _q.QuoteID, NULL [InvoiceID], _q.ClientID, _q.SiteID, _q.ProjectID
                FROM Quote _q WITH (NOLOCK)
                WHERE
                    _q.QuoteID = pf.QuoteID

                UNION ALL

                SELECT 3 [RowType], NULL [JobID], NULL [JobNo], NULL [QuoteID], _i.InvoiceID, _i.ClientID, (SELECT TOP 1 _ii.SiteID FROM InvoiceItem _ii WITH (NOLOCK) INNER JOIN #SiteIdData _siid ON _ii.SiteID = _siid.SiteID WHERE _ii.InvoiceID = _i.InvoiceID) [SiteID], _i.ProjectID
                FROM
                    Invoice _i WITH (NOLOCK)
                WHERE
                    _i.InvoiceID = pf.InvoiceID
            ) j
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'PDF'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter. NOTE: Done twice in this part of SQL to improve speed!
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Register')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Register r WITH (NOLOCK) ON pat.DataID = r.RegisterID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Register'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Floorplan')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Floorplan f WITH (NOLOCK) ON pat.DataID = f.FloorplanID
            INNER JOIN Register r WITH (NOLOCK) ON f.RegisterID = r.RegisterID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Floorplan'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Room')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Room rm WITH (NOLOCK) ON pat.DataID = rm.RoomID
            INNER JOIN Register r WITH (NOLOCK) ON rm.RegisterID = r.RegisterID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Room'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Sample')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Sample s WITH (NOLOCK) ON pat.DataID = s.SampleID
            INNER JOIN Room rm WITH (NOLOCK) ON s.RoomID = rm.RoomID
            INNER JOIN Register r WITH (NOLOCK) ON rm.RegisterID = r.RegisterID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Sample'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'SampleDocument')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN SampleDocument sd WITH (NOLOCK) ON pat.DataID = sd.SampleDocumentID
            INNER JOIN Sample s WITH (NOLOCK) ON sd.SampleID = s.SampleID
            INNER JOIN Room rm WITH (NOLOCK) ON s.RoomID = rm.RoomID
            INNER JOIN Register r WITH (NOLOCK) ON rm.RegisterID = r.RegisterID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'SampleDocument'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaOutletPortalData')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN LegionellaOutletPortalData lopd WITH (NOLOCK) ON pat.DataID = lopd.LegionellaOutletPortalDataID
            INNER JOIN Job j WITH (NOLOCK) ON lopd.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'LegionellaOutletPortalData'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaTaskEvent')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN LegionellaTaskEvent lte WITH (NOLOCK) ON pat.DataID = lte.LegionellaTaskEventID
            INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lte.LegionellaTaskID = lt.LegionellaTaskID
            CROSS APPLY
            (
                SELECT
                    1 [RowType],
                    _l.LegionellaID,
                    NULL [LegionellaAssetOutletID],
                    NULL [LegionellaLocationID]
                FROM
                    Legionella _l WITH (NOLOCK)
                WHERE
                    _l.LegionellaID = lt.LegionellaID

                UNION ALL

                SELECT
                    2 [RowType],
                    _la.LegionellaID,
                    _la.LegionellaAssetID [LegionellaAssetOutletID],
                    NULL [LegionellaLocationID]
                FROM
                    LegionellaAsset _la WITH (NOLOCK)
                WHERE
                    _la.LegionellaAssetID = lt.LegionellaAssetID
                        AND
                    _la.Deleted IS NULL

                UNION ALL

                SELECT
                    3 [RowType],
                    _ll.LegionellaID,
                    NULL [LegionellaAssetOutletID],
                    _ll.LegionellaLocationID
                FROM
                    LegionellaLocation _ll WITH (NOLOCK)
                WHERE
                    _ll.LegionellaLocationID = lt.LegionellaLocationID
                        AND
                    _ll.Deleted IS NULL

                UNION ALL

                SELECT
                    4 [RowType],
                    _ll.LegionellaID,
                    _lo.LegionellaOutletID [LegionellaAssetOutletID],
                    _ll.LegionellaLocationID
                FROM
                    LegionellaLocation _ll WITH (NOLOCK)
                    INNER JOIN LegionellaOutlet _lo WITH (NOLOCK) ON _ll.LegionellaLocationID = _lo.LegionellaLocationID
                WHERE
                    _lo.LegionellaOutletID = lt.LegionellaOutletID
                        AND
                    _ll.Deleted IS NULL
                        AND
                    _lo.Deleted IS NULL
            ) lao
            INNER JOIN Legionella l WITH (NOLOCK) ON lao.LegionellaID = l.LegionellaID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN #SiteIdData siid ON j.SiteID = siid.SiteID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'LegionellaTaskEvent'
                AND
            CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    /* 3. Tables that need validation based upon the Site. For these, we just have to join to the current User's Sites to check for access. */
    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Site')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            NULL [JobNo],
            si.Address,
            si.Postcode,
            si.UPRN,
			NULL
        FROM
            @PortalAuditTrailData pat
            INNER JOIN Site si WITH (NOLOCK) ON pat.DataID = si.SiteID
            INNER JOIN #SiteIdData siid ON si.SiteID = siid.SiteID
			LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID	
			LEFT JOIN Project p WITH (NOLOCK) ON p.ProjectID = ps.ProjectID
        WHERE
            pat.DataTable = 'Site'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            si.Address,
            si.Postcode,
            si.UPRN
			
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'SiteDocument')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            NULL [JobNo],
            si.Address,
            si.Postcode,
            si.UPRN,
			NULL
        FROM
            @PortalAuditTrailData pat
            INNER JOIN SiteDocument sid WITH (NOLOCK) ON pat.DataID = sid.SiteDocumentID
            INNER JOIN Site si WITH (NOLOCK) ON sid.SiteID = si.SiteID
            INNER JOIN #SiteIdData siid ON si.SiteID = siid.SiteID
			LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID	
			LEFT JOIN Project p WITH (NOLOCK) ON p.ProjectID = ps.ProjectID
        WHERE
            pat.DataTable = 'SiteDocument'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            si.Address,
            si.Postcode,
            si.UPRN
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'SiteLegionellaTemperatureLimits')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            NULL [JobNo],
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN SiteLegionellaTemperatureLimits siltl WITH (NOLOCK) ON pat.DataID = siltl.SiteLegionellaTemperatureLimitsID
            INNER JOIN Site si WITH (NOLOCK) ON siltl.SiteID = si.SiteID
            INNER JOIN #SiteIdData siid ON si.SiteID = siid.SiteID
			LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID	
			LEFT JOIN Project p WITH (NOLOCK) ON p.ProjectID = ps.ProjectID
        WHERE
            pat.DataTable = 'SiteLegionellaTemperatureLimits'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'PortalUserAutoEmail')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            NULL [JobNo],
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
        FROM
            @PortalAuditTrailData pat
            INNER JOIN PortalUserAutoEmail puae WITH (NOLOCK) ON pat.DataID = puae.PortalUserAutoEmailID
            INNER JOIN Site si WITH (NOLOCK) ON puae.SiteID = si.SiteID
            INNER JOIN #SiteIdData siid ON si.SiteID = siid.SiteID
			LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID	
			LEFT JOIN Project p WITH (NOLOCK) ON p.ProjectID = ps.ProjectID
        WHERE
            pat.DataTable = 'PortalUserAutoEmail'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            si.Address,
            si.Postcode,
            si.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

	-- LegionellaTask
	IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaTask')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ': ' + ISNULL(lt.RiskDescription, 'N/A') + ' for ' + COALESCE(TaskAsset.Type, TaskLegionella.Type, TaskOutlet.Type, TaskOutletLocation.Type) + ': ' + COALESCE(TaskAsset.Name, TaskOutlet.Name, TaskOutletLocation.Name, TaskLegionella.Name, 'No Building Designation'),
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            COALESCE(TaskAsset.JobNo, TaskLegionella.JobNo, TaskOutlet.JobNo, TaskOutletLocation.JobNo),
            COALESCE(TaskAsset.Address, TaskLegionella.Address, TaskOutlet.Address, TaskOutletLocation.Address),
            COALESCE(TaskAsset.Postcode, TaskLegionella.Postcode, TaskOutlet.Postcode, TaskOutletLocation.Postcode),
            COALESCE(TaskAsset.UPRN, TaskLegionella.UPRN, TaskOutlet.UPRN, TaskOutletLocation.UPRN),
			COALESCE(TaskAsset.Project, TaskLegionella.Project, TaskOutlet.Project, TaskOutletLocation.Project)
        FROM
            @PortalAuditTrailData pat
			INNER JOIN LegionellaTask lt WITH (NOLOCK) ON pat.DataID = lt.LegionellaTaskID

			OUTER APPLY
			(
				SELECT
					j.JobNo,
					s.Address,
					s.Postcode,
					s.UPRN,
					p.Project + ' (' + p.GroupName + ')' [Project],
					'Asset' [Type],
					la.SystemRef [Name]
				FROM LegionellaAsset la
					INNER JOIN Legionella l WITH (NOLOCK) ON la.LegionellaID = l.LegionellaID
					INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
					INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
					INNER JOIN Site s WITH (NOLOCK) ON j.SiteID = s.SiteID
					LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
				WHERE
					la.LegionellaAssetID = lt.LegionellaAssetID
			) [TaskAsset]

			OUTER APPLY	
			(
				SELECT
					j2.JobNo,
					s2.Address,
					s2.Postcode,
					s2.UPRN,
					p2.Project + ' (' + p2.GroupName + ')' [Project],
					'Building' [Type],
					l2.BuildingDesignation [Name]
				FROM Legionella l2
					INNER JOIN JobEmployee je2 WITH (NOLOCK) ON l2.JobEmployeeID = je2.JobEmployeeID
					INNER JOIN Job j2 WITH (NOLOCK) ON je2.JobID = j2.JobID
					INNER JOIN Site s2 WITH (NOLOCK) ON j2.SiteID = s2.SiteID
					LEFT JOIN Project p2 WITH (NOLOCK) ON j2.ProjectID = p2.ProjectID
				WHERE
					l2.LegionellaID = lt.LegionellaID
			) [TaskLegionella]

			OUTER APPLY	
			(
				SELECT
					j3.JobNo,
					s3.Address,
					s3.Postcode,
					s3.UPRN,
					p3.Project + ' (' + p3.GroupName + ')' [Project],
					'Outlet' [Type],
					lo.SystemRef [Name]
				FROM LegionellaOutlet lo
					INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON lo.LegionellaLocationID = ll.LegionellaLocationID
					INNER JOIN Legionella l3 WITH (NOLOCK) ON ll.LegionellaID = l3.LegionellaID
					INNER JOIN JobEmployee je3 WITH (NOLOCK) ON l3.JobEmployeeID = je3.JobEmployeeID
					INNER JOIN Job j3 WITH (NOLOCK) ON je3.JobID = j3.JobID
					INNER JOIN Site s3 WITH (NOLOCK) ON j3.SiteID = s3.SiteID
					LEFT JOIN Project p3 WITH (NOLOCK) ON j3.ProjectID = p3.ProjectID
				WHERE
					lo.LegionellaOutletID = lt.LegionellaOutletID
			) [TaskOutlet]
			
			OUTER APPLY	
			(
				SELECT
					j4.JobNo,
					s4.Address,
					s4.Postcode,
					s4.UPRN,
					p4.Project + ' (' + p4.GroupName + ')' [Project],
					'Location' [Type],
					ll2.Location [Name]
				FROM LegionellaLocation ll2
					INNER JOIN Legionella l4 WITH (NOLOCK) ON ll2.LegionellaID = l4.LegionellaID
					INNER JOIN JobEmployee je4 WITH (NOLOCK) ON l4.JobEmployeeID = je4.JobEmployeeID
					INNER JOIN Job j4 WITH (NOLOCK) ON je4.JobID = j4.JobID
					INNER JOIN Site s4 WITH (NOLOCK) ON j4.SiteID = s4.SiteID
					LEFT JOIN Project p4 WITH (NOLOCK) ON j4.ProjectID = p4.ProjectID
				WHERE
					ll2.LegionellaLocationID = lt.LegionellaLocationID
			) [TaskOutletLocation]
        WHERE
            pat.DataTable = 'LegionellaTask'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ': ' + ISNULL(lt.RiskDescription, 'N/A') + ' for ' + COALESCE(TaskAsset.Type, TaskLegionella.Type, TaskOutlet.Type, TaskOutletLocation.Type) + ': ' + COALESCE(TaskAsset.Name, TaskOutlet.Name, TaskOutletLocation.Name, TaskLegionella.Name, 'No Building Designation'),
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            COALESCE(TaskAsset.JobNo, TaskLegionella.JobNo, TaskOutlet.JobNo, TaskOutletLocation.JobNo),
            COALESCE(TaskAsset.Address, TaskLegionella.Address, TaskOutlet.Address, TaskOutletLocation.Address),
            COALESCE(TaskAsset.Postcode, TaskLegionella.Postcode, TaskOutlet.Postcode, TaskOutletLocation.Postcode),
            COALESCE(TaskAsset.UPRN, TaskLegionella.UPRN, TaskOutlet.UPRN, TaskOutletLocation.UPRN),
			COALESCE(TaskAsset.Project, TaskLegionella.Project, TaskOutlet.Project, TaskOutletLocation.Project)
    END

	-- LegionellaAsset
	IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaAsset')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            CASE
				WHEN pat.Message like '%Asset Category%'
				THEN pat.Message
				ELSE pat.Message + ' for Asset: ' + la.SystemRef
			END,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')' [Project]
        FROM
            @PortalAuditTrailData pat
			INNER JOIN LegionellaAsset la WITH (NOLOCK) ON pat.DataID = la.LegionellaAssetID
			INNER JOIN Legionella l WITH (NOLOCK) ON la.LegionellaID = l.LegionellaID
			INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
			INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
			INNER JOIN Site s WITH (NOLOCK) ON j.SiteID = s.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'LegionellaAsset'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            CASE
				WHEN pat.Message like '%Asset Category%'
				THEN pat.Message
				ELSE pat.Message + ' for Asset: ' + la.SystemRef
			END,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

	-- LegionellaAsset
	IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'Legionella')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Building: ' + ISNULL(l.BuildingDesignation, 'No Building Designation'),
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')' [Project]
        FROM
            @PortalAuditTrailData pat
			INNER JOIN Legionella l WITH (NOLOCK) ON pat.DataID = l.LegionellaID
			INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
			INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
			INNER JOIN Site s WITH (NOLOCK) ON j.SiteID = s.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'Legionella'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Building: ' + ISNULL(l.BuildingDesignation, 'No Building Designation'),
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

	-- LegionellaLocation
	IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaLocation')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Outlet Location: ' + ll.Location,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')' [Project]
        FROM
            @PortalAuditTrailData pat
			INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON pat.DataID = ll.LegionellaLocationID
			INNER JOIN Legionella l WITH (NOLOCK) ON ll.LegionellaID = l.LegionellaID
			INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
			INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
			INNER JOIN Site s WITH (NOLOCK) ON j.SiteID = s.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'LegionellaLocation'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Outlet Location: ' + ll.Location,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

	-- LegionellaOutlet
	IF EXISTS(SELECT 1 FROM @PortalAuditTrailData WHERE DataTable = 'LegionellaOutlet')
    BEGIN
        INSERT INTO @MainData (PortalAuditTrailID, UserID, [User], Message, DataTable, DataID, DataIPAddress, DateCreated, Company, JobNo, Address, Postcode, UPRN, Project)
        SELECT
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Outlet: ' + lo.SystemRef,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')' [Project]
        FROM
            @PortalAuditTrailData pat
			INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON pat.DataID = lo.LegionellaOutletID
			INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON lo.LegionellaLocationID = ll.LegionellaLocationID
			INNER JOIN Legionella l WITH (NOLOCK) ON ll.LegionellaID = l.LegionellaID
			INNER JOIN JobEmployee je WITH (NOLOCK) ON l.JobEmployeeID = je.JobEmployeeID
			INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
			INNER JOIN Site s WITH (NOLOCK) ON j.SiteID = s.SiteID
			LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        WHERE
            pat.DataTable = 'LegionellaOutlet'
        GROUP BY
            pat.PortalAuditTrailID,
            pat.UserID,
            pat.[User],
            pat.Message + ' for Outlet: ' + lo.SystemRef,
            pat.DataTable,
            pat.DataID,
            pat.DataIPAddress,
            pat.DateCreated,
            pat.Company,
            j.JobNo,
            s.Address,
            s.Postcode,
            s.UPRN,
			p.Project + ' (' + p.GroupName + ')'
    END

    -- Start the main SELECT.
    SELECT
        m.PortalAuditTrailID,
        m.UserID,
        m.[User],
        m.Message,
        m.DataIPAddress,
        m.DateCreated [DateAccessed],
        m.Company,
        m.JobNo,
        m.Address,
        m.Postcode,
        m.UPRN,
		m.Project
    FROM @MainData m
    WHERE
        CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
            THEN 1
            ELSE CASE WHEN m.UPRN = @LocUPRN THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
            THEN 1
            ELSE
                CASE WHEN m.Address LIKE '%' + @LocAddressSearchString + '%' OR m.Postcode LIKE '%' + @LocAddressSearchString + '%' OR m.Address + ', ' + ISNULL(m.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR m.UPRN = @LocAddressSearchString
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @LocJobNo IS NULL -- JobNo Filter.
            THEN 1
            ELSE CASE WHEN m.JobNo = @LocJobNo THEN 1 ELSE 0 END
        END = 1
    ORDER BY
        m.DateCreated DESC

    -- Clear up temp tables.
    DROP TABLE #ClientIdData
    DROP TABLE #SiteIdData


    SET NOCOUNT OFF;
END
GO

ALTER PROCEDURE [dbo].[GetPortalHomeTabGraphData]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @JobID INT = NULL,
    @ReturnAsChart BIT = NULL
/**********************************************************************
** Overview: Get data which is used by the graphs on the Home tab of the Portal. Currently just Risk and Recommended Action items.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), ''),
        @JobID = NULLIF(@JobID, 0),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    DECLARE @SurveyTypeIdData TABLE (SurveyTypeID INT PRIMARY KEY)
    INSERT INTO @SurveyTypeIdData (SurveyTypeID)
    SELECT s
    FROM dbo.SplitString(@SurveyTypeIDs, ',')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get Register data up front to reduce table scans.
    CREATE TABLE #RegisterData (ClientID INT, SiteID INT, JobID INT, JobNo INT, RegisterID INT, BuildingDesignation VARCHAR(1000), RegisterFinish DATETIME, SurveyTypeID INT)

    -- Add an index on important #RegisterData fields to increase speed below.
    CREATE INDEX temp_RegisterData ON #RegisterData (RegisterID)

    INSERT INTO #RegisterData (ClientID, SiteID, JobID, JobNo, RegisterID, BuildingDesignation, RegisterFinish, SurveyTypeID)
    SELECT
        csd.ClientID,
        csd.SiteID,
        j.JobID,
        j.JobNo,
        r.RegisterID,
        r.BuildingDesignation,
        r.RegisterFinish,
        su.SurveyTypeID
    FROM
        @ClientSiteData csd
        INNER JOIN Job j WITH (NOLOCK) ON csd.ClientID = j.ClientID AND csd.SiteID = j.SiteID AND j.Cancelled IS NULL AND j.Approved IS NOT NULL
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN @SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
    WHERE
        (@JobID IS NULL OR j.JobID = @JobID)
    GROUP BY
        csd.ClientID,
        csd.SiteID,
        j.JobID,
        j.JobNo,
        r.RegisterID,
        r.BuildingDesignation,
        r.RegisterFinish,
        su.SurveyTypeID

    -- Get SampleComputedData data up front to reduce table scans on the SampleComputedData table.
    DECLARE @SiteSampleComputedData TABLE (SampleComputedDataID INT, RegisterID INT, SampleID INT, SampleResult INT, RecommendedAction NVARCHAR(100), RecommendedActionColour VARCHAR(6), RecommendedActionSortOrder INT, RiskScore INT, RiskScoreGroupID INT, RiskScoreGroup VARCHAR(100), RiskScoreGroupColour VARCHAR(10), RiskScoreSortOrder INT)

    INSERT INTO @SiteSampleComputedData (SampleComputedDataID, RegisterID, SampleID, SampleResult, RecommendedAction, RecommendedActionColour, RecommendedActionSortOrder, RiskScore, RiskScoreGroupID, RiskScoreGroup, RiskScoreGroupColour, RiskScoreSortOrder)
    SELECT
        scd.SampleComputedDataID,
        r.RegisterID,
        scd.SampleID,
        scd.SampleResult,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        scd.RecommendedActionSortOrder,
        scd.RiskScore,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupColour,
        scd.RiskScoreSortOrder
    FROM
        #RegisterData r
        INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
        INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        INNER JOIN GuidSamples gs WITH (NOLOCK) ON s.SampleID = gs.SampleID
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON gs.SampleID = scd.SampleID AND gs.ClientID = scd.ClientID AND gs.SiteID = scd.SiteID
    WHERE
        scd.Removed = 0
            AND
        scd.RecommendedAction IS NOT NULL

    -- Clear up temp tables.
    DROP TABLE #RegisterData

    -- Get all Risk Items data up front to reduce table scans.
    DECLARE @RiskItemsData TABLE (SampleID INT NOT NULL, RiskScoreGroupID INT NOT NULL, RiskScoreGroup VARCHAR(100) NOT NULL, RiskScoreGroupColour VARCHAR(10) NOT NULL, RiskScoreSortOrder INT NOT NULL)
    INSERT INTO @RiskItemsData (SampleID, RiskScoreGroupID, RiskScoreGroup, RiskScoreGroupColour, RiskScoreSortOrder)
    SELECT
        SampleID,
        ISNULL(RiskScoreGroupID, -1) [RiskScoreGroupID],
        ISNULL(RiskScoreGroup, 'Inaccessible') [RiskScoreGroup],
        ISNULL(RiskScoreGroupColour, '#CCCCCC') [RiskScoreGroupColour],
        ISNULL(RiskScoreSortOrder, 0) [RiskScoreSortOrder]
    FROM @SiteSampleComputedData
    WHERE SampleResult <> 0
    GROUP BY
        SampleID,
        ISNULL(RiskScoreGroupID, -1),
        ISNULL(RiskScoreGroup, 'Inaccessible'),
        ISNULL(RiskScoreGroupColour, '#CCCCCC'),
        ISNULL(RiskScoreSortOrder, 0)

    -- Get all Recommended Action Items data up front to reduce table scans.
    DECLARE @RecommendedActionItemsData TABLE (RegisterID INT NOT NULL, SampleID INT NOT NULL, RecommendedAction NVARCHAR(100) NOT NULL, RecommendedActionColour VARCHAR(6) NOT NULL, RecommendedActionSortOrder INT NOT NULL)
    INSERT INTO @RecommendedActionItemsData (RegisterID, SampleID, RecommendedAction, RecommendedActionColour, RecommendedActionSortOrder)
    SELECT
        RegisterID,
        SampleID,
        RecommendedAction,
        RecommendedActionColour,
        ISNULL(RecommendedActionSortOrder, 0) [RecommendedActionSortOrder]
    FROM @SiteSampleComputedData
    GROUP BY
        RegisterID,
        SampleID,
        RecommendedAction,
        RecommendedActionColour,
        ISNULL(RecommendedActionSortOrder, 0)

    -- Get the total number of items.
    DECLARE @TotalRiskItems INT = (SELECT COUNT(*) FROM @RiskItemsData)
    DECLARE @TotalRecActionItems INT = (SELECT COUNT(*) FROM @RecommendedActionItemsData)

    -- Start the main SELECTs.
    IF @ReturnAsChart = 1
    BEGIN
        -- Get the Max Sort Order, so we can re-arrange the items.
        DECLARE @MaxRisksSortOrder INT = (SELECT MAX(SortOrder) FROM RiskScoreGroup WITH (NOLOCK))
        DECLARE @RecommendedActionsAllowed INT = (SELECT mc.MobileConfigInt FROM MobileConfig mc WITH (NOLOCK) INNER JOIN MobileConfigType mct WITH (NOLOCK) ON mc.MobileConfigTypeID = mct.MobileConfigTypeID WHERE mct.ConfigType = 'RecommendedActionAllowed')

        -- Start the SELECT to get Risk Items data.
        SELECT
            rsg.RiskScoreGroup [category],
            rsg.RiskScoreGroupColour [Colour],
            COUNT(rid.SampleID) [Share],
            @TotalRiskItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            @MaxRisksSortOrder - MAX(rsg.RiskScoreSortOrder) + 1 [SortOrder]
        FROM
            (
                SELECT RiskScoreGroupID, ScoreGroup [RiskScoreGroup], Colour [RiskScoreGroupColour], SortOrder [RiskScoreSortOrder] FROM RiskScoreGroup WITH (NOLOCK)
                UNION ALL
                SELECT -1 [RiskScoreGroupID], 'Inaccessible' [RiskScoreGroup], '#CCCCCC' [RiskScoreGroupColour], CAST(-1 AS INT) [RiskScoreSortOrder]
            ) rsg
            LEFT JOIN @RiskItemsData rid ON rsg.RiskScoreGroupID = ISNULL(rid.RiskScoreGroupID, -1)
        GROUP BY
            rsg.RiskScoreGroup,
            rsg.RiskScoreGroupColour
        ORDER BY
            SortOrder,
            category

        -- Start the SELECT to get Recommended Action Items data.
        SELECT
            ra.RecommendedAction [category],
            '#' + ra.RecommendedActionColour [Colour],
            COUNT(raid.SampleID) [Share],
            @TotalRecActionItems [TotalItems],
            CAST(0 AS BIT) [VisibleInLegend],
            MAX(ra.RecommendedActionSortOrder) [SortOrder]
        FROM
            (
                SELECT
                    ra.RecommendedAction,
                    ra.RecommendedActionColour,
                    CASE WHEN ra.RecommendedAction = 'No further action required'
                        THEN 0
                        ELSE ra.RecommendedActionSortOrder + 1
                    END [RecommendedActionSortOrder]
                FROM
                (
                    SELECT
                        e20.ShortDescription [RecommendedAction],
                        rac.RecommendedActionColour,
						CASE WHEN e20.ShortDescription = 'No further action required'
                        THEN 0
                        ELSE @RecommendedActionsAllowed - e20.ElementIntValue 
					END[RecommendedActionSortOrder]

                    FROM
                        ElementIntMeaning e20 WITH (NOLOCK)
                        INNER JOIN RecommendedActionColour rac WITH (NOLOCK) ON e20.ElementIntValue = rac.ElementIntValue
                    WHERE e20.ElementTypeID = 20
                    UNION
                    SELECT
                        'No further action required' [RecommendedAction],
                        '92A0F4' [RecommendedActionColour],
                        0 [RecommendedActionSortOrder]
                ) ra
                WHERE ra.RecommendedActionSortOrder >= 0
            ) ra
            LEFT JOIN @RecommendedActionItemsData raid ON ra.RecommendedAction = raid.RecommendedAction
        GROUP BY
            ra.RecommendedAction,
            ra.RecommendedActionColour
        ORDER BY
            category,
            SortOrder
    END
    ELSE
    BEGIN
        -- Start the SELECT to get Risk Items data.
        SELECT
            SampleID,
            RiskScoreGroup,
            RiskScoreGroupColour,
            RiskScoreSortOrder
        FROM @RiskItemsData
        GROUP BY
            SampleID,
            RiskScoreGroup,
            RiskScoreGroupColour,
            RiskScoreSortOrder

        -- Start the SELECT to get Recommended Action Items data.
        SELECT
            RegisterID,
            SampleID,
            RecommendedAction,
            RecommendedActionColour,
            ISNULL(RecommendedActionSortOrder, 0) [RecommendedActionSortOrder]
        FROM @RecommendedActionItemsData
        GROUP BY
            RegisterID,
            SampleID,
            RecommendedAction,
            RecommendedActionColour,
            ISNULL(RecommendedActionSortOrder, 0)
    END


    SET NOCOUNT OFF;
END

GO

ALTER PROCEDURE [dbo].[GetPortalJobDataForSites]
    @PortalUserID INT = 0,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @SurveyTypeIDs VARCHAR(MAX) = ''
/**********************************************************************
** Overview: For a list of Sites, get job data about each Site. There should be one row for each Site.
**           This is designed for a small number of SiteID's
** E.g. Does a Survey, Bulk Sample Certificate, Air Test or Legionella job exist for each Site?
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Get information that is repeated per row in variables to reduce table scans.
    DECLARE @Sites BIT, @Surveys BIT, @BulkSamples BIT, @AirTests BIT, @Legionella BIT, @b__onlyshowApprovedAirTestsOnPortal BIT, @b__PortalShowPDFsOnSitesTab BIT, @MoreThanOneSite BIT
    SELECT
        @Sites = pu.Sites,
        @Surveys = pu.Surveys,
        @BulkSamples = pu.BulkSamples,
        @AirTests = pu.AirTests,
        @Legionella = pu.Legionella,
        @b__onlyshowApprovedAirTestsOnPortal = cfg.b__onlyshowApprovedAirTestsOnPortal,
        @b__PortalShowPDFsOnSitesTab = cfg.b__PortalShowPDFsOnSitesTab,
        @MoreThanOneSite = CASE WHEN @SiteIDs IS NULL THEN 1 ELSE CASE WHEN CHARINDEX(',', @SiteIDs) > 0 THEN 1 ELSE 0 END END
    FROM
        PortalUser pu WITH (NOLOCK)
        CROSS JOIN Config cfg WITH (NOLOCK)
    WHERE
        pu.PortalUserID = @PortalUserID

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    CREATE TABLE #SurveyTypeIdData (SurveyTypeID INT PRIMARY KEY)
    INSERT INTO #SurveyTypeIdData (SurveyTypeID)
    SELECT s
    FROM dbo.SplitString(@SurveyTypeIDs, ',')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT, UseRiskColours BIT)
    INSERT INTO @ClientSiteData (ClientID, SiteID, UseRiskColours)
    SELECT
        c.ClientID,
        si.SiteID,
        c.UseRiskColours
    FROM
        #ClientIdData cid
        INNER JOIN Client c WITH (NOLOCK) ON cid.ClientID = c.ClientID
        INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
        INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
        INNER JOIN (
            SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
        ) sis ON si.SiteID = sis.SiteID
    WHERE
        si.Deleted IS NULL
    GROUP BY
        c.ClientID,
        si.SiteID,
        c.UseRiskColours

    -- Get Job/Appointment data up front to reduce table scans.
    DECLARE @JobAppointmentData TABLE (ClientID INT, SiteID INT, UseRiskColours BIT, JobID INT)
    INSERT INTO @JobAppointmentData (ClientID, SiteID, UseRiskColours, JobID)
    SELECT
        csd.ClientID,
        csd.SiteID,
        csd.UseRiskColours,
        j.JobID
    FROM
        @ClientSiteData csd
        INNER JOIN Job j WITH (NOLOCK) ON csd.ClientID = j.ClientID AND csd.SiteID = j.SiteID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        LEFT JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID
        LEFT JOIN AppointmentAirMonitoring aam WITH (NOLOCK) ON a.AppointmentID = aam.AppointmentID
    WHERE
        a.DateDeclined IS NULL
            AND
        j.Cancelled IS NULL
            AND
        (j.Approved IS NOT NULL OR (j.Approved IS NULL AND aam.AppointmentAirMonitoringID IS NOT NULL))
    GROUP BY
        csd.ClientID,
        csd.SiteID,
        csd.UseRiskColours,
        j.JobID

    -- Get all Site Jobs up front to reduce table scans (get the most recent job for each Site).
    CREATE TABLE #SiteJobs (SiteID INT PRIMARY KEY, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SitePost2000 BIT, UnmanagedSite BIT, IsSiteDocument BIT, JobID INT, MaxRegisterFinish DATETIME, HighestRiskScoreGroupID INT, FirstNextReviewDate INT, ReinspectionDate DATETIME, Surveyed INT, DateOfNextReviewIsAsRequired INT, AllNegative INT)

    -- Add an index on important #SiteJobs fields to increase speed below.
    CREATE INDEX temp_SiteJobs ON #SiteJobs (SiteID)

    INSERT INTO #SiteJobs (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish)
    SELECT SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, RegisterFinish [MaxRegisterFinish]
    FROM
    (
        SELECT
            si.SiteID,
            si.Address [SiteAddress],
            si.Postcode [SitePostcode],
            si.Post2000 [SitePost2000],
            si.UnmanagedSite,
            jd.IsSiteDocument,
            jd.JobID,
            jd.RegisterFinish,
            ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.RegisterFinish DESC, jd.JobID DESC) [RowID]
        FROM
            @ClientSiteData csd
            INNER JOIN (
                SELECT
                    a.*,
                    ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.RegisterFinish DESC, a.JobID DESC) [RowID]
                FROM
                (
                    SELECT 0 [IsSiteDocument], jad.ClientID, jad.SiteID, jad.JobID, r.RegisterFinish
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON jad.JobID = je.JobID
                        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                        INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                    UNION ALL
                    SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], sidi.WorkDate [RegisterFinish]
                    FROM
                        SiteDocument sid WITH (NOLOCK)
                        INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                    WHERE
                        sid.SiteDocumentTypeID = 3 -- Surveys
                            AND
                        sid.Deleted IS NULL
                ) a
            ) jd ON
                CASE WHEN jd.IsSiteDocument = 1
                    THEN -1
                    ELSE csd.ClientID
                END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.Post2000,
            si.UnmanagedSite,
            jd.IsSiteDocument,
            jd.JobID,
            jd.RegisterFinish,
            jd.RowID
    ) a
    WHERE a.RowID = 1
    GROUP BY
        a.SiteID,
        a.SiteAddress,
        a.SitePostcode,
        a.SitePost2000,
        a.UnmanagedSite,
        a.IsSiteDocument,
        a.JobID,
        a.RegisterFinish,
        a.RowID
    ORDER BY
        a.SiteID

    -- Execute the SPROC PopulateSiteJobsReinspectionState. This populates additional columns in #SiteJobs, so it assumes #SiteJobs already exists.
    EXEC PopulateSiteJobsReinspectionState


    -- Start the main SELECT.
    SELECT
        csd.SiteID,
        ISNULL(SUM(o.SurveyJobCount), 0) [SurveyJobCount], -- How many Survey's against the current Site? That is Approved, appointment not declined, quote not rejected, etc.
        ISNULL(SUM(o.SurveyDocCount), 0) [SurveyDocCount], -- How many Survey Site Docs are there for the current Site?
        ISNULL(SUM(o.ManagementPlanCount), 0) [ManagementPlanCount], -- How many Management Plans are there for the current Site?
        ISNULL(SUM(o.BulkSampleCertificateCount), 0) [BulkSampleCertificateCount], -- How many Bulk Sample Report's against the current Site (either as part of a Survey or a standalone)? That is Approved, quote not rejected, etc.
        ISNULL(SUM(o.BulkSampleDocCount), 0) [BulkSampleDocCount], -- How many Bulk Sample Site Docs are there for the current Site?
        ISNULL(SUM(o.AirTestCount), 0) [AirTestCount], -- How many Air test's against the current Site? That is Approved (if config @b__onlyshowApprovedAirTestsOnPortal is true), appointment not declined, quote not rejected, etc.
        ISNULL(SUM(o.AirTestDocCount), 0) [AirTestDocCount], -- How many Air Test Site Docs are there for the current Site?
        ISNULL(SUM(o.LegionellaJobCount), 0) [LegionellaJobCount], -- How many Legionella's against the current Site? That is Approved, appointment not declined, quote not rejected, etc.
        ISNULL(SUM(o.LegionellaDocCount), 0) [LegionellaDocCount], -- How many Legionella Site Docs are there for the current Site?
        MAX(o.SurveyJobFileName) [SurveyJobFileName], -- If the job has 1 Survey, get the PDF File Name for it.
        MAX(o.SurveySiteDocumentID) [SurveySiteDocumentID], -- Get the latest Survey Site Document.
        MAX(o.SurveyDocFileName) [SurveyDocFileName], -- If the job has 1 Survey Site Document, get it.
        MAX(o.ManagementPlanSiteDocumentID) [ManagementPlanSiteDocumentID], -- Get the latest Site Document Management Plan.
        MAX(o.ManagementPlanFileName) [ManagementPlanFileName], -- If the job has 1 Management Plan, get it.
        MAX(o.BulkSampleJobFileName) [BulkSampleJobFileName], -- If the job has 1 BSR, get the PDF File Name for it.
        MAX(o.BulkSampleSiteDocumentID) [BulkSampleSiteDocumentID], -- Get the latest Bulk Sample Site Document.
        MAX(o.BulkSampleDocFileName) [BulkSampleDocFileName], -- If the job has 1 Bulk Sample Site Document, get it.
        MAX(o.AirTestJobFileName) [AirTestJobFileName], -- If the job has 1 Air Test, get the PDF File Name for it.
        MAX(o.AirTestSiteDocumentID) [AirTestSiteDocumentID], -- Get the latest Air Test Site Document.
        MAX(o.AirTestDocFileName) [AirTestDocFileName], -- If the job has 1 Air Test Site Document, get it.
        MAX(o.LegionellaJobFileName) [LegionellaJobFileName], -- If the job has 1 Legionella job, get the PDF File Name for it.
        MAX(o.LegionellaSiteDocumentID) [LegionellaSiteDocumentID], -- Get the latest Legionella Site Document.
        MAX(o.LegionellaDocFileName) [LegionellaDocFileName], -- If the job has 1 Legionella Site Document, get it.
        MAX(o.ExternalPhotoID) [ExternalPhotoID], -- Get the ExternalPhotoID of a photo that has data. Attempt to get a normal Register/Legionella record PhotoID.
        MAX(o.PhotoSiteDocumentID) [PhotoSiteDocumentID], -- Get the SiteDocumentID of a document that is a photo.
        MAX(o.SurveyJobID) [SurveyJobID], -- The JobID of the latest Survey on the Site (previously ordered by RegisterFinish DESC).
        MIN(o.SurveyReinspectionDate) [SurveyReinspectionDate], -- The first Reinspection Date of the latest Survey at the Site.
        MAX(o.SurveySampleResultsOverview) [SurveySampleResultsOverview], -- The Sample Results Overview of the Site.
		CASE
			WHEN si.Post2000 = 1 OR si.UnmanagedSite = 1
			THEN 1
		ELSE
			0
		END [NotRequired],
		CASE
			WHEN MIN(o.DateOfNextReviewIsAsRequired) > 0
			THEN 1
		ELSE
			0
		END [AsRequired],
		ISNULL(MIN(o.AllNegative),0) [AllNegative]
    FROM
        @ClientSiteData csd
		INNER JOIN Site si ON csd.SiteID = si.SiteID
        INNER JOIN
        (
            SELECT -- Surveys
                su.SiteID,
                su.SurveyJobCount,
                NULL [SurveyDocCount],
                NULL [ManagementPlanCount],
                NULL [BulkSampleCertificateCount],
                NULL [BulkSampleDocCount],
                NULL [AirTestCount],
                NULL [AirTestDocCount],
                NULL [LegionellaJobCount],
                NULL [LegionellaDocCount],
                su.ExternalPhotoID,
                su.JobID [SurveyJobID],
                CASE WHEN @Surveys = 1 AND (su.SurveyJobCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN (
                        SELECT TOP 1 _pf.FileName
                        FROM PDF _pf WITH (NOLOCK)
                        WHERE
                            _pf.JobID = su.JobID
                                AND
                            _pf.DateDeleted IS NULL
                                AND
                            _pf.FileName NOT LIKE '%bsr%'
                                AND
                            _pf.FileName NOT LIKE '%ra%'
                                AND
                            _pf.FileName NOT LIKE '%asb5%'
                        ORDER BY
                            _pf.DateCreated DESC
                    )
                    ELSE NULL
                END [SurveyJobFileName],
                NULL [SurveySiteDocumentID],
                NULL [SurveyDocFileName],
                NULL [SurveyReinspectionDate],
                NULL [SurveySampleResultsOverview],
                NULL [ManagementPlanSiteDocumentID],
                NULL [ManagementPlanFileName],
                NULL [BulkSampleJobFileName],
                NULL [BulkSampleSiteDocumentID],
                NULL [BulkSampleDocFileName],
                NULL [AirTestJobFileName],
                NULL [AirTestSiteDocumentID],
                NULL [AirTestDocFileName],
                NULL [LegionellaJobFileName],
                NULL [LegionellaSiteDocumentID],
                NULL [LegionellaDocFileName],
                NULL [PhotoSiteDocumentID],
				NULL				[DateOfNextReviewIsAsRequired],
				NULL [AllNegative]
            FROM
            (
                SELECT
                    main.SiteID,
                    COUNT(DISTINCT main.JobID) [SurveyJobCount],
                    MAX(mep.ExternalPhotoID) [ExternalPhotoID],
                    MAX(main.JobID) [JobID]
                FROM
                (
                    SELECT
                        jad.SiteID,
                        jad.JobID,
                        su.SurveyTypeID
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON jad.JobID = je.JobID
                        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                        INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                ) main
                OUTER APPLY
                (
                    SELECT TOP 1 p.PhotoID [ExternalPhotoID]
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON jad.JobID = je.JobID
                        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                        INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                        INNER JOIN Photo p WITH (NOLOCK) ON r.PhotoID = p.PhotoID
                    WHERE
                        jad.SiteID = main.SiteID
                    ORDER BY
                        r.MainExternalPhoto DESC,
                        p.PhotoNo DESC,
                        p.PhotoID DESC
                ) mep
                GROUP BY
                    main.SiteID
            ) su
            UNION ALL
            SELECT -- Bulk Samples
                bsr.SiteID,
                NULL [SurveyJobCount],
                NULL [SurveyDocCount],
                NULL [ManagementPlanCount],
                bsr.BSRCount [BulkSampleCertificateCount],
                NULL [BulkSampleDocCount],
                NULL [AirTestCount],
                NULL [AirTestDocCount],
                NULL [LegionellaJobCount],
                NULL [LegionellaDocCount],
                NULL [ExternalPhotoID],
                NULL [SurveyJobID],
                NULL [SurveyJobFileName],
                NULL [SurveySiteDocumentID],
                NULL [SurveyDocFileName],
                NULL [SurveyReinspectionDate],
                NULL [SurveySampleResultsOverview],
                NULL [ManagementPlanSiteDocumentID],
                NULL [ManagementPlanFileName],
                CASE WHEN @BulkSamples = 1 AND (bsr.BSRCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN bsr.FileName
                    ELSE NULL
                END [BulkSampleJobFileName],
                NULL [BulkSampleSiteDocumentID],
                NULL [BulkSampleDocFileName],
                NULL [AirTestJobFileName],
                NULL [AirTestSiteDocumentID],
                NULL [AirTestDocFileName],
                NULL [LegionellaJobFileName],
                NULL [LegionellaSiteDocumentID],
                NULL [LegionellaDocFileName],
                NULL [PhotoSiteDocumentID],
				NULL				[DateOfNextReviewIsAsRequired],
				NULL [AllNegative]
            FROM
            (
                SELECT
                    main.SiteID,
                    COUNT(DISTINCT main.JobID) [BSRCount],
                    MAX(main.FileName) [FileName]
                FROM
                (
                    SELECT
                        jad.SiteID,
                        jad.JobID,
                        bsr.PDFId,
                        bsr.FileName
                    FROM
                        @JobAppointmentData jad
                        OUTER APPLY
                        (
                            SELECT TOP 1 _pf.PdfID, _pf.FileName
                            FROM PDF _pf WITH (NOLOCK)
                            WHERE
                                _pf.JobID = jad.JobID
                                    AND
                                _pf.DateDeleted IS NULL
                                    AND
                                _pf.FileName LIKE '%bsr%'
                            ORDER BY
                                _pf.DateCreated DESC
                        ) bsr
                    WHERE
                        bsr.PDFId IS NOT NULL
                ) main
                GROUP BY
                    main.SiteID
            ) bsr
            UNION ALL
            SELECT -- Air Tests
                at.SiteID,
                NULL [SurveyJobCount],
                NULL [SurveyDocCount],
                NULL [ManagementPlanCount],
                NULL [BulkSampleCertificateCount],
                NULL [BulkSampleDocCount],
                at.AirTestCount,
                NULL [AirTestDocCount],
                NULL [LegionellaJobCount],
                NULL [LegionellaDocCount],
                NULL [ExternalPhotoID],
                NULL [SurveyJobID],
                NULL [SurveyJobFileName],
                NULL [SurveySiteDocumentID],
                NULL [SurveyDocFileName],
                NULL [SurveyReinspectionDate],
                NULL [SurveySampleResultsOverview],
                NULL [ManagementPlanSiteDocumentID],
                NULL [ManagementPlanFileName],
                NULL [BulkSampleJobFileName],
                NULL [BulkSampleSiteDocumentID],
                NULL [BulkSampleDocFileName],
                CASE WHEN @AirTests = 1 AND (at.AirTestCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN at.FileName
                    ELSE NULL
                END [AirTestJobFileName],
                NULL [AirTestSiteDocumentID],
                NULL [AirTestDocFileName],
                NULL [LegionellaJobFileName],
                NULL [LegionellaSiteDocumentID],
                NULL [LegionellaDocFileName],
                NULL [PhotoSiteDocumentID],
				NULL				[DateOfNextReviewIsAsRequired],
				NULL [AllNegative]
            FROM
            (
                SELECT
                    main.SiteID,
                    COUNT(DISTINCT main.AirTestID) [AirTestCount],
                    MAX(main.FileName) [FileName]
                FROM
                (
                    SELECT
                        jad.SiteID,
                        jad.JobID,
                        at.AirTestID,
                        pf.FileName
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON jad.JobID = je.JobID
                        INNER JOIN Airtest at WITH (NOLOCK) ON je.JobEmployeeID = at.JobEmployeeID
                        OUTER APPLY
                        (
                            SELECT TOP 1 _pf.FileName
                            FROM PDF _pf WITH (NOLOCK)
                            WHERE
                                _pf.JobID = jad.JobID
                                    AND
                                _pf.DateDeleted IS NULL
                                    AND
                                _pf.FileName NOT LIKE '%bsr%'
                                    AND
                                _pf.FileName NOT LIKE '%ra%'
                                    AND
                                _pf.FileName NOT LIKE '%asb5%'
                                    AND
                                _pf.FileName LIKE '%\_' + CAST(at.AirTestID AS VARCHAR(20)) + ' (%' ESCAPE '\'
                            ORDER BY
                                _pf.DateCreated DESC
                        ) pf
                    WHERE
                        CASE WHEN @b__onlyshowApprovedAirTestsOnPortal = 1
                            THEN at.OfficeApproved
                            ELSE 1
                        END = 1
                ) main
                GROUP BY
                    main.SiteID
            ) at
            UNION ALL
            SELECT -- Legionella
                l.SiteID,
                NULL [SurveyJobCount],
                NULL [SurveyDocCount],
                NULL [ManagementPlanCount],
                NULL [BulkSampleCertificateCount],
                NULL [BulkSampleDocCount],
                NULL [AirTestCount],
                NULL [AirTestDocCount],
                l.LegionellaJobCount,
                NULL [LegionellaDocCount],
                CASE WHEN @Legionella = 1 THEN l.ExternalPhotoID ELSE NULL END [ExternalPhotoID],
                NULL [SurveyJobID],
                NULL [SurveyJobFileName],
                NULL [SurveySiteDocumentID],
                NULL [SurveyDocFileName],
                NULL [SurveyReinspectionDate],
                NULL [SurveySampleResultsOverview],
                NULL [ManagementPlanSiteDocumentID],
                NULL [ManagementPlanFileName],
                NULL [BulkSampleJobFileName],
                NULL [BulkSampleSiteDocumentID],
                NULL [BulkSampleDocFileName],
                NULL [AirTestJobFileName],
                NULL [AirTestSiteDocumentID],
                NULL [AirTestDocFileName],
                CASE WHEN @Legionella = 1 AND (l.LegionellaJobCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN (
                        SELECT TOP 1 _pf.FileName
                        FROM PDF _pf WITH (NOLOCK)
                        WHERE
                            _pf.JobID = l.JobID
                                AND
                            _pf.DateDeleted IS NULL
                                AND
                            _pf.FileName NOT LIKE '%bsr%'
                                AND
                            _pf.FileName NOT LIKE '%ra%'
                                AND
                            _pf.FileName NOT LIKE '%asb5%'
                        ORDER BY
                            _pf.DateCreated DESC
                        )
                    ELSE NULL
                END [LegionellaJobFileName],
                NULL [LegionellaSiteDocumentID],
                NULL [LegionellaDocFileName],
                NULL [PhotoSiteDocumentID],
				NULL				[DateOfNextReviewIsAsRequired],
				NULL [AllNegative]
            FROM
            (
                SELECT
                    main.SiteID,
                    COUNT(DISTINCT main.JobID) [LegionellaJobCount],
                    MAX(main.JobID) [JobID],
                    MAX(main.ExternalPhotoID) [ExternalPhotoID]
                FROM
                (
                    SELECT
                        jad.SiteID,
                        jad.JobID,
                        p.PhotoID [ExternalPhotoID]
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON jad.JobID = je.JobID
                        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
                        LEFT JOIN Photo p WITH (NOLOCK) ON l.PhotoID = p.PhotoID
                ) main
                GROUP BY
                    main.SiteID
            ) l
            UNION ALL
            SELECT -- Site Documents with a Type
                sid.SiteID,
                NULL [SurveyJobCount],
                sid.SurveyDocCount,
                sid.ManagementPlanCount,
                NULL [BulkSampleCertificateCount],
                sid.BulkSampleDocCount,
                NULL [AirTestCount],
                sid.AirTestDocCount,
                NULL [LegionellaJobCount],
                sid.LegionellaDocCount,
                NULL [ExternalPhotoID],
                NULL [SurveyJobID],
                NULL [SurveyJobFileName],
                CASE WHEN @Surveys = 1 AND (sid.SurveyDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.SurveySiteDocumentID
                    ELSE NULL
                END [SurveySiteDocumentID],
                CASE WHEN @Surveys = 1 AND (sid.SurveyDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.SurveyDocFileName
                    ELSE NULL
                END [SurveyDocFileName],
                NULL [SurveyReinspectionDate],
                NULL [SurveySampleResultsOverview],
                CASE WHEN @Surveys = 1 AND (sid.ManagementPlanCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.ManagementPlanSiteDocumentID
                    ELSE NULL
                END [ManagementPlanSiteDocumentID],
                CASE WHEN @Surveys = 1 AND (sid.ManagementPlanCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.ManagementPlanFileName
                    ELSE NULL
                END [ManagementPlanFileName],
                NULL [BulkSampleJobFileName],
                CASE WHEN @BulkSamples = 1 AND (sid.BulkSampleDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.BulkSampleSiteDocumentID
                    ELSE NULL
                END [BulkSampleSiteDocumentID],
                CASE WHEN @BulkSamples = 1 AND (sid.BulkSampleDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.BulkSampleDocFileName
                    ELSE NULL
                END [BulkSampleDocFileName],
                NULL [AirTestJobFileName],
                CASE WHEN @AirTests = 1 AND (sid.AirTestDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.AirTestSiteDocumentID
                    ELSE NULL
                END [AirTestSiteDocumentID],
                CASE WHEN @AirTests = 1 AND (sid.AirTestDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.AirTestDocFileName
                    ELSE NULL
                END [AirTestDocFileName],
                NULL [LegionellaJobFileName],
                CASE WHEN @Legionella = 1 AND (sid.LegionellaDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.LegionellaSiteDocumentID
                    ELSE NULL
                END [LegionellaSiteDocumentID],
                CASE WHEN @Legionella = 1 AND (sid.LegionellaDocCount = 1 OR @b__PortalShowPDFsOnSitesTab = 1)
                    THEN sid.LegionellaDocFileName
                    ELSE NULL
                END [LegionellaDocFileName],
                CASE WHEN @Sites = 1
                    THEN sid.PhotoSiteDocumentID
                    ELSE NULL
                END [PhotoSiteDocumentID],
				NULL				[DateOfNextReviewIsAsRequired],
				NULL [AllNegative]
            FROM
            (
                SELECT
                    main.SiteID,
                    COUNT(DISTINCT main.SurveySiteDocumentID) [SurveyDocCount],
                    MAX(main.SurveySiteDocumentID) [SurveySiteDocumentID],
                    MAX(main.SurveyFileName) [SurveyDocFileName],
                    COUNT(DISTINCT main.ManagementPlanSiteDocumentID) [ManagementPlanCount],
                    MAX(main.ManagementPlanSiteDocumentID) [ManagementPlanSiteDocumentID],
                    MAX(main.ManagementPlanFileName) [ManagementPlanFileName],
                    COUNT(DISTINCT main.BulkSampleSiteDocumentID) [BulkSampleDocCount],
                    MAX(main.BulkSampleSiteDocumentID) [BulkSampleSiteDocumentID],
                    MAX(main.BulkSampleFileName) [BulkSampleDocFileName],
                    COUNT(DISTINCT main.AirTestSiteDocumentID) [AirTestDocCount],
                    MAX(main.AirTestSiteDocumentID) [AirTestSiteDocumentID],
                    MAX(main.AirTestFileName) [AirTestDocFileName],
                    COUNT(DISTINCT main.LegionellaSiteDocumentID) [LegionellaDocCount],
                    MAX(main.LegionellaSiteDocumentID) [LegionellaSiteDocumentID],
                    MAX(main.LegionellaFileName) [LegionellaDocFileName],
                    MAX(main.PhotoSiteDocumentID) [PhotoSiteDocumentID]
                FROM
                (
                    SELECT
                        csd.SiteID,
                        suSid.SiteDocumentID [SurveySiteDocumentID],
                        suSid.FileName [SurveyFileName],
                        mPlanSid.SiteDocumentID [ManagementPlanSiteDocumentID],
                        mPlanSid.FileName [ManagementPlanFileName],
                        bsrSid.SiteDocumentID [BulkSampleSiteDocumentID],
                        bsrSid.FileName [BulkSampleFileName],
                        atSid.SiteDocumentID [AirTestSiteDocumentID],
                        atSid.FileName [AirTestFileName],
                        lSid.SiteDocumentID [LegionellaSiteDocumentID],
                        lSid.FileName [LegionellaFileName],
                        pSid.SiteDocumentID [PhotoSiteDocumentID]
                    FROM
                        @ClientSiteData csd
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID,
                                sid.FileName
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 3
                        ) suSid
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID,
                                sid.FileName
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 2
                        ) mPlanSid
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID,
                                sid.FileName
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 4
                        ) bsrSid
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID,
                                sid.FileName
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 5
                        ) atSid
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID,
                                sid.FileName
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 6
                        ) lSid
                        OUTER APPLY
                        (
                            SELECT
                                sid.SiteDocumentID
                            FROM
                                SiteDocument sid WITH (NOLOCK)
                            WHERE
                                sid.SiteID = csd.SiteID
                                    AND
                                sid.Deleted IS NULL
                                    AND
                                sid.SiteDocumentTypeID = 7
                        ) pSid
                ) main
                WHERE
                    main.SurveySiteDocumentID IS NOT NULL OR main.ManagementPlanSiteDocumentID IS NOT NULL OR main.BulkSampleSiteDocumentID IS NOT NULL OR main.AirTestSiteDocumentID IS NOT NULL OR main.LegionellaSiteDocumentID IS NOT NULL OR main.PhotoSiteDocumentID IS NOT NULL
                GROUP BY
                    main.SiteID
            ) sid
            UNION ALL
            SELECT -- Survey Sample Results Overview HTML and Reinspection Dates
                ssro.SiteID,
                NULL [SurveyJobCount],
                NULL [SurveyDocCount],
                NULL [ManagementPlanCount],
                NULL [BulkSampleCertificateCount],
                NULL [BulkSampleDocCount],
                NULL [AirTestCount],
                NULL [AirTestDocCount],
                NULL [LegionellaJobCount],
                NULL [LegionellaDocCount],
                NULL [ExternalPhotoID],
                NULL [SurveyJobID],
                NULL [SurveyJobFileName],
                NULL [SurveySiteDocumentID],
                NULL [SurveyDocFileName],
                ssro.ReinspectionDate [SurveyReinspectionDate],
                ssro.SampleResultsOverview [SurveySampleResultsOverview],
                NULL [ManagementPlanSiteDocumentID],
                NULL [ManagementPlanFileName],
                NULL [BulkSampleJobFileName],
                NULL [BulkSampleSiteDocumentID],
                NULL [BulkSampleDocFileName],
                NULL [AirTestJobFileName],
                NULL [AirTestSiteDocumentID],
                NULL [AirTestDocFileName],
                NULL [LegionellaJobFileName],
                NULL [LegionellaSiteDocumentID],
                NULL [LegionellaDocFileName],
                NULL [PhotoSiteDocumentID],
				ssro.DateOfNextReviewIsAsRequired [DateOfNextReviewIsAsRequired],
				ssro.AllNegative
            FROM
            (
                SELECT
                    main.SiteID,
                    MIN(main.ReinspectionDate) [ReinspectionDate],
                    MAX(main.SampleResultsOverview) [SampleResultsOverview],
					main.DateOfNextReviewIsAsRequired,
					main.AllNegative
                FROM
                (
                    SELECT
                        sj.SiteID,
                        sj.ReinspectionDate,
                        CASE WHEN jad.UseRiskColours = 1
                            THEN ssros.RiskSampleResultsOverview
                            ELSE ssros.RecommendedActionSampleResultsOverview
                        END [SampleResultsOverview],
						sj.DateOfNextReviewIsAsRequired,
						sj.AllNegative
                    FROM
                        @JobAppointmentData jad
                        INNER JOIN #SiteJobs sj ON jad.SiteID = sj.SiteID
						INNER JOIN Site si ON sj.SiteID = si.SiteID
                        LEFT JOIN SiteSampleResultsOverviewSorting ssros WITH (NOLOCK) ON jad.ClientID = ssros.ClientID AND jad.SiteID = ssros.SiteID
                ) main
                GROUP BY
                    main.SiteID,
					main.DateOfNextReviewIsAsRequired,
					main.AllNegative
            ) ssro
        ) o ON csd.SiteID = o.SiteID
    GROUP BY
        csd.SiteID,
		si.Post2000,
		si.UnmanagedSite		
	ORDER BY
        csd.SiteID


    -- Clear up temp tables.
    DROP TABLE #ClientIdData
    DROP TABLE #SurveyTypeIdData
    DROP TABLE #SiteJobs


    SET NOCOUNT OFF;
END






GO

/*
    SEARCH SPROCS
*/
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalSamplesForJobSearch')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalSamplesForJobSearch] AS BEGIN SET NOCOUNT ON; END')
END
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalSitesForSearch')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalSitesForSearch] AS BEGIN SET NOCOUNT ON; END')
END
GO

ALTER PROCEDURE [dbo].[GetPortalSamplesForJobSearch]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX),
    @JobID INT
/**********************************************************************
** Overview: Get Samples for a Job for the Portal Search. For example, when displaying
** a single Job in the search results, we show a list of Sample References at the bottom.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Get information that is repeated per row in variables to reduce table scans.
    DECLARE @Sites BIT, @Surveys BIT, @IsSurvey BIT
    SELECT
        @Sites = pu.Sites,
        @Surveys = pu.Surveys,
        @IsSurvey = CASE WHEN EXISTS(
            SELECT 1
            FROM
                JobEmployee je WITH (NOLOCK)
                INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID
                INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
            WHERE
                je.JobID = @JobID
            )
            THEN 1
            ELSE 0
        END
    FROM
        PortalUser pu WITH (NOLOCK)
    WHERE
        pu.PortalUserID = @PortalUserID

    -- If we have access to the Sites tab and the current job is a Survey, then we need to get the latest GUID Version of each Sample (but only the SampleID, as a seperate column).
    IF @Sites = 1 AND @IsSurvey = 1
    BEGIN
        -- Get all Clients into a table variable.
        DECLARE @ClientData TABLE (ClientID INT PRIMARY KEY)
        INSERT INTO @ClientData (ClientID)
        SELECT s
        FROM dbo.SplitString(@ClientIDs, ',')

        -- Get the SiteID for the Job.
        DECLARE @SiteID INT = (
            SELECT SiteID
            FROM Job WITH (NOLOCK)
            WHERE JobID = @JobID
        )

        -- Get the Samples for the Site into a table variable.
        DECLARE @SamplesData TABLE (JobID INT NOT NULL, JobNo INT NOT NULL, SiteID INT NOT NULL, SampleID INT NOT NULL, SampleRef VARCHAR(50), AsSample BIT NOT NULL, GUID VARCHAR(50), GUIDVersion INT)
        INSERT INTO @SamplesData (JobID, JobNo, SiteID, SampleID, SampleRef, AsSample, GUID, GUIDVersion)
        SELECT
            j.JobID,
            j.JobNo,
            j.SiteID,
            s.SampleID,
            s.SampleRef,
            s.AsSample,
            s.GUID,
            s.GUIDVersion
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN @ClientData c ON j.ClientID = c.ClientID
            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
            INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
            INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
            INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        WHERE
            j.SiteID = @SiteID
                AND
            je.MainEmployee = 1
                AND
            r.DateApproved IS NOT NULL
                AND
            NULLIF(s.SampleRef, '') IS NOT NULL

        -- Start the main SELECT.
        SELECT
            s.JobID,
            s.JobNo,
            s.SiteID,
            @IsSurvey [IsSurvey],
            s.SampleID,
            s.SampleRef,
            RTRIM(LTRIM(dbo.RemoveReinspectionRefFromSamplRef(s.SampleRef))) [SampleRefFormatted],
            s.AsSample,
            ISNULL(sd.SampleID, s.SampleID) [LatestSampleID]
        FROM
            @SamplesData s
            LEFT JOIN (
                SELECT
                    s.JobID,
                    s.JobNo,
                    s.SiteID,
                    s.SampleID,
                    s.SampleRef,
                    s.GUID,
                    s.GUIDVersion,
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(s.GUID, NEWID()) ORDER BY s.GUIDVersion DESC) [RowID]
                FROM @SamplesData s
            ) sd ON s.GUID = sd.GUID AND sd.RowID = 1
        WHERE s.JobID = @JobID
        GROUP BY
            s.JobID,
            s.JobNo,
            s.SiteID,
            s.SampleID,
            s.SampleRef,
            s.AsSample,
            sd.SampleID
        ORDER BY
            s.SampleRef,
            s.AsSample
    END
    ELSE
    BEGIN -- User has access to the Surveys section or we are getting Samples for a Bulk Sample Job.
        SELECT
            j.JobID,
            j.JobNo,
            j.SiteID,
            @IsSurvey [IsSurvey],
            s.SampleID,
            s.SampleRef,
            RTRIM(LTRIM(dbo.RemoveReinspectionRefFromSamplRef(s.SampleRef))) [SampleRefFormatted],
            s.AsSample,
            s.SampleID [LatestSampleID]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID
            LEFT JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
            INNER JOIN Room rm WITH (NOLOCK) ON r.RegisterID = rm.RegisterID
            INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        WHERE
            je.JobID = @JobID
                AND
            je.MainEmployee = 1
                AND
            r.DateApproved IS NOT NULL
                AND
            NULLIF(s.SampleRef, '') IS NOT NULL
                AND
            CASE WHEN @IsSurvey = 0 -- Make sure we join to the Floorplan table when a Survey (to prevent data errors).
                THEN 1
                ELSE CASE WHEN f.FloorplanID IS NOT NULL AND f.FloorplanID = rm.FloorplanID THEN 1 ELSE 0 END
            END = 1
        ORDER BY
            s.SampleRef,
            s.AsSample
    END


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetPortalSitesForSearch]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX),
    @ProjectGroupID INT,
    @ProjectID INT,
    @SiteIDs VARCHAR(MAX),
    @AddressSearchString VARCHAR(200),
    @RoomName VARCHAR(50)
/**********************************************************************
** Overview: Gets Sites when performing a Search. We find the Sites based upon the Address or the Room Name.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @RoomName = NULLIF(LTRIM(RTRIM(@RoomName)), '')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            (
                SELECT LTRIM(RTRIM(s)) [ClientID] FROM dbo.SplitString(@ClientIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            (
                SELECT LTRIM(RTRIM(s)) [ClientID] FROM dbo.SplitString(@ClientIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
    END

    -- Get Sites for the Search string(s) passed in. Get Sites by Address matching, followed by Room Name matching.
    DECLARE @SitesForSearchData TABLE (IsRoom BIT NOT NULL, SiteID INT NOT NULL PRIMARY KEY, Address VARCHAR(200) NOT NULL, Postcode VARCHAR(10) NOT NULL, UPRN VARCHAR(50), SurveyExists BIT NOT NULL, RoomID INT, RoomDescription VARCHAR(50), RoomCode VARCHAR(MAX), RoomGUID VARCHAR(50), RoomGUIDVersion INT)

    IF @AddressSearchString IS NOT NULL -- Address matching.
    BEGIN
        INSERT INTO @SitesForSearchData (IsRoom, SiteID, Address, Postcode, UPRN, SurveyExists, RoomID, RoomDescription, RoomCode, RoomGUID, RoomGUIDVersion)
        SELECT
            0 [IsRoom],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            CASE WHEN EXISTS(
                SELECT 1
                FROM
                    Job _j WITH (NOLOCK)
                    INNER JOIN Quote _q WITH (NOLOCK) ON _j.JobID = _q.JobID AND _q.Rejected IS NULL
                    INNER JOIN Appointment _a WITH (NOLOCK) ON _q.QuoteID = _a.QuoteID AND _a.DateDeclined IS NULL
                    INNER JOIN JobEmployee _je WITH (NOLOCK) ON _j.JobID = _je.JobID
                    INNER JOIN Register _r WITH (NOLOCK) ON _je.JobEmployeeID = _r.JobEmployeeID AND _r.DateApproved IS NOT NULL
                    INNER JOIN Survey _su WITH (NOLOCK) ON _r.SurveyID = _su.SurveyID
                WHERE
                    _j.SiteID = si.SiteID
                        AND
                    _j.Cancelled IS NULL
            )
                THEN 1
                ELSE 0
            END [SurveyExists],
            NULL [RoomID],
            NULL [RoomDescription],
            NULL [RoomCode],
            NULL [RoomGUID],
            NULL [RoomGUIDVersion]
        FROM
            @ClientSiteData csd
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
            OUTER APPLY
            (
                SELECT TOP 1
                    j.JobID,
                    j.ClientOrderNo
                FROM
                    Job j WITH (NOLOCK)
                WHERE
                    j.ClientID = csd.ClientID
                        AND
                    j.SiteID = si.SiteID
                        AND
                    j.Cancelled IS NULL
                        AND
                    j.Approved IS NOT NULL
                ORDER BY
                    j.Approved DESC
            ) j
        WHERE
            (si.Address LIKE '%' + @AddressSearchString + '%')
                OR
            (si.PostCode LIKE '%' + @AddressSearchString + '%')
                OR
            (si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @AddressSearchString + '%')
                OR
            (si.UPRN = @AddressSearchString)
                OR
            (j.ClientOrderNo = @AddressSearchString)
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN
        ORDER BY
            si.SiteID,
            si.Address,
            si.Postcode
    END

    IF @RoomName IS NOT NULL -- Room Name matching.
    BEGIN
        INSERT INTO @SitesForSearchData (IsRoom, SiteID, Address, Postcode, UPRN, SurveyExists, RoomID, RoomDescription, RoomCode, RoomGUID, RoomGUIDVersion)
        SELECT
            1 [IsRoom],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            1 [SurveyExists],
            MAX(rm.RoomID) [RoomID],
            MAX(RTRIM(rm.Description)) [RoomDescription],
            MAX(NULLIF(rm.RoomCode, '')) [RoomCode],
            MAX(rm.GUID) [RoomGUID],
            MAX(rm.GUIDVersion) [RoomGUIDVersion]
        FROM
            @ClientSiteData csd
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
            INNER JOIN Job j WITH (NOLOCK) ON si.SiteID = j.SiteID AND j.Cancelled IS NULL
            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
            LEFT JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
            INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
            INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        WHERE
            si.SiteID NOT IN (SELECT SiteID FROM @SitesForSearchData)
                AND
            a.DateDeclined IS NULL
                AND
            (
                RTRIM(rm.Description) LIKE '%' + @RoomName + '%'
                    OR
                ISNULL(rm.RoomCode, '') LIKE '%' + @RoomName + '%'
            )
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN
        ORDER BY
            si.SiteID,
            si.Address,
            si.Postcode
    END

    -- Start the main SELECT.
    SELECT *
    FROM @SitesForSearchData sfsd
    ORDER BY
        sfsd.IsRoom,
        sfsd.Address,
        sfsd.Postcode,
        sfsd.RoomDescription,
        sfsd.RoomCode


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetSamples]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX),
    @JobID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @RegisterID INT = NULL,
    @FloorplanID INT = NULL,
    @RoomID INT = NULL,
    @SampleID INT = NULL,
    @RoomDescription VARCHAR(100) = '', -- This is a Description filter, not just for Rooms but Sample Ref's, Descriptions, etc.
    @RoomNumber INT = NULL, -- This is a Number filter, not just for Rooms but Register Item Numbers as well.
    @GetLastNoteCreated BIT = NULL, -- Whether or not we want to get when the last note was created. This is a performance hit, so only get it when we need it.
    @UniqueItemCode VARCHAR(20) = NULL, -- A unique item code, passed in via QR Codes. From this, we return a treeview ID for the latest item with the same GUID, which gets selected in the treeview.
    @RiskID INT = NULL, -- Filter to Samples with this RiskID only?
    @RecAction VARCHAR(100) = NULL -- Filter to Samples with this Recommended Action only?
/**********************************************************************
** Overview: Get samples for the portal site/survey info page
** NOTE: Only used as of initial revision by the portal for computed data
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @JobID = NULLIF(@JobID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @RegisterID = NULLIF(@RegisterID, 0),
        @FloorplanID = NULLIF(@FloorplanID, 0),
        @RoomID = NULLIF(@RoomID, 0),
        @SampleID = NULLIF(@SampleID, 0),
        @RoomDescription = NULLIF(LTRIM(RTRIM(@RoomDescription)), ''),
        @RoomNumber = NULLIF(@RoomNumber, 0),
        @GetLastNoteCreated = ISNULL(@GetLastNoteCreated, 1),
        @UniqueItemCode = NULLIF(@UniqueItemCode, ''),
        @RiskID = NULLIF(@RiskID, 0),
        @RecAction = NULLIF(REPLACE(REPLACE(REPLACE(ISNULL(@RecAction, ''), ' ', '_'), '&', ''), ',', ''), '')

    -- Set IDs lower down the tree if IDs higher up the tree are set.
    IF @SampleID IS NOT NULL AND @RoomID IS NULL
    BEGIN
        SELECT @RoomID = RoomID FROM Sample WITH (NOLOCK) WHERE SampleID = @SampleID
    END
    IF @RoomID IS NOT NULL AND @FloorplanID IS NULL
    BEGIN
        SELECT @FloorplanID = FloorplanID FROM Room WITH (NOLOCK) WHERE RoomID = @RoomID
    END
    IF @FloorplanID IS NOT NULL AND @RegisterID IS NULL
    BEGIN
        SELECT @RegisterID = RegisterID FROM Floorplan WITH (NOLOCK) WHERE FloorplanID = @FloorplanID
    END
    IF (@JobID IS NULL AND @SiteIDs IS NULL AND @RegisterID IS NULL AND @FloorplanID IS NULL AND @RoomID IS NULL AND @SampleID IS NULL)
    BEGIN
        RAISERROR ('No params passed to GetSamples.', 16, 1)
        RETURN;
    END

    -- Get all Clients into a table variable.
    DECLARE @ClientData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientData (ClientID)
    SELECT s
    FROM dbo.SplitString(@ClientIDs, ',')

    -- Get all Sites into a table variable.
    DECLARE @SiteData TABLE (SiteID INT PRIMARY KEY)
    IF @JobID IS NULL
    BEGIN
        INSERT INTO @SiteData (SiteID)
        SELECT s
        FROM dbo.SplitString(@SiteIDs, ',')
    END

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @BasementName VARCHAR(50)
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @BasementName = ISNULL(NULLIF(cfg.s__BasementName, ''), 'Z-Sub Level')
    FROM
        Config cfg WITH (NOLOCK)

    -- Table for getting all the related GUIDs if the @RegisterID is passed in as a filter.
    DECLARE @RegisterIDs TABLE (RegisterID INT NOT NULL PRIMARY KEY)
    DECLARE @RegisterGUID VARCHAR(50)
    IF @RegisterID IS NOT NULL
    BEGIN
        SELECT @RegisterGUID = (SELECT GUID FROM Register WITH (NOLOCK) WHERE RegisterID = @RegisterID)
        IF @RegisterGUID IS NULL
        BEGIN
            INSERT INTO @RegisterIDs VALUES (@RegisterID)
        END
        ELSE
        BEGIN
            INSERT INTO @RegisterIDs SELECT RegisterID FROM Register WITH (NOLOCK) WHERE GUID = @RegisterGUID
        END
    END

    -- Get Register data up front to reduce the main SELECT table scans.
    CREATE TABLE #RegisterData (JobID INT, JobNo INT, JobEmployeeID INT, EmployeeID INT, RegisterID INT, BuildingDesignation VARCHAR(1000), GUID VARCHAR(50), GUIDVersion INT, SurveyStartDate DATETIME, SurveyFinishDate DATETIME, SurveyType VARCHAR(100))

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_RegisterData_ID ON #RegisterData (JobEmployeeID, RegisterID)
    CREATE INDEX tempIdx_RegisterData_GUID ON #RegisterData (GUID, GUIDVersion DESC)

    INSERT INTO #RegisterData (JobID, JobNo, JobEmployeeID, EmployeeID, RegisterID, BuildingDesignation, GUID, GUIDVersion, SurveyStartDate, SurveyFinishDate, SurveyType)
    SELECT
        j.JobID,
        j.JobNo,
        je.JobEmployeeID,
        je.EmployeeID,
        r.RegisterID,
        r.BuildingDesignation,
        r.GUID,
        r.GUIDVersion,
        CAST(MIN(r.RegisterStart) AS DATE) [SurveyStartDate],
        CAST(MIN(r.RegisterFinish) AS DATE) [SurveyFinishDate],
        sut.Description [SurveyType]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientData c ON j.ClientID = c.ClientID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN SurveyType sut WITH (NOLOCK) ON su.SurveyTypeID = sut.SurveyTypeID
    WHERE
        (@JobID IS NULL OR j.JobID = @JobID)
            AND
        (@JobID IS NOT NULL OR j.SiteID IN (SELECT SiteID FROM @SiteData))
            AND
        j.Cancelled IS NULL
            AND
        j.Approved IS NOT NULL
            AND
        (@RegisterID IS NULL OR r.RegisterID IN (SELECT RegisterID FROM @RegisterIDs))
    GROUP BY
        j.JobID,
        j.JobNo,
        je.JobEmployeeID,
        je.EmployeeID,
        r.RegisterID,
        r.BuildingDesignation,
        r.GUID,
        r.GUIDVersion,
        sut.Description

    -- Table for getting all the related GUIDs if the @FloorplanID is passed in as a filter.
    DECLARE @FloorplanIDs TABLE (FloorplanID INT NOT NULL PRIMARY KEY)
    DECLARE @FloorplanGUID VARCHAR(50)
    IF @FloorplanID IS NOT NULL
    BEGIN
        SELECT @FloorplanGUID = (SELECT GUID FROM Floorplan WITH (NOLOCK) WHERE FloorplanID = @FloorplanID)
        IF @FloorplanGUID IS NULL
        BEGIN
            INSERT INTO @FloorplanIDs VALUES (@FloorplanID)
        END
        ELSE
        BEGIN
            INSERT INTO @FloorplanIDs SELECT FloorplanID FROM Floorplan WITH (NOLOCK) WHERE GUID = @FloorplanGUID
        END
    END

    -- Get Floorplan data up front to reduce the main SELECT table scans.
    CREATE TABLE #FloorplanData (RegisterID INT, BuildingDesignation VARCHAR(1000), FloorplanID INT, FloorNumber INT, Description VARCHAR(MAX), TEAMS_StoreID INT, HasFloorplan BIT, GUID VARCHAR(50), GUIDVersion INT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_FloorplanData_ID ON #FloorplanData (RegisterID, FloorplanID)
    CREATE INDEX tempIdx_FloorplanData_GUID ON #FloorplanData (GUID, GUIDVersion DESC)

    INSERT INTO #FloorplanData (RegisterID, BuildingDesignation, FloorplanID, FloorNumber, Description, TEAMS_StoreID, HasFloorplan, GUID, GUIDVersion)
    SELECT
        r.RegisterID,
        r.BuildingDesignation,
        f.FloorplanID,
        f.FloorNumber,
        ISNULL(f.DescriptionOverride, REPLACE(dbo.FloorName(f.FloorNumber), 'Z-Sub Level', @BasementName)) [Description],
        f.TEAMS_StoreID,
        CASE WHEN f.FloorplanData IS NOT NULL THEN 1 ELSE CASE WHEN f.AutocadData IS NOT NULL THEN 1 ELSE 0 END END [HasFloorplan],
        f.GUID,
        f.GUIDVersion
    FROM
        #RegisterData r
        INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
    WHERE
        (@FloorplanID IS NULL OR f.FloorplanID IN (SELECT FloorplanID FROM @FloorplanIDs))

    -- Table for getting all the related GUIDs if the @RoomID is passed in as a filter.
    DECLARE @RoomIDs TABLE (RoomID INT NOT NULL PRIMARY KEY)
    DECLARE @RoomGUID VARCHAR(50)
    IF @RoomID IS NOT NULL
    BEGIN
        SELECT @RoomGUID = (SELECT GUID FROM Room WITH (NOLOCK) WHERE RoomID = @RoomID)
        IF @RoomGUID IS NULL
        BEGIN
            INSERT INTO @RoomIDs VALUES (@RoomID)
        END
        ELSE
        BEGIN
            INSERT INTO @RoomIDs SELECT RoomID FROM Room WITH (NOLOCK) WHERE GUID = @RoomGUID
        END
    END

    -- Get Room data up front to reduce the main SELECT table scans.
    CREATE TABLE #RoomData (FloorplanID INT, RoomID INT, Description VARCHAR(200), Number INT, RoomCode VARCHAR(MAX), RoomDisplay VARCHAR(MAX), GUID VARCHAR(50), GUIDVersion INT, ContainsFilteredData BIT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_RoomData_ID ON #RoomData (FloorplanID, RoomID, ContainsFilteredData)
    CREATE INDEX tempIdx_RoomData_GUID ON #RoomData (GUID, GUIDVersion DESC)

    INSERT INTO #RoomData (FloorplanID, RoomID, Description, Number, RoomCode, RoomDisplay, GUID, GUIDVersion, ContainsFilteredData)
    SELECT
        f.FloorplanID,
        rm.RoomID,
        RTRIM(rm.Description) [Description],
        rm.Number,
        NULLIF(rm.RoomCode, '') [RoomCode],
        rmd.RoomDisplay,
        rm.GUID,
        rm.GUIDVersion,
        CASE WHEN @RoomNumber IS NULL AND @RoomDescription IS NULL
            THEN 1
            ELSE
                CASE WHEN rm.Number = @RoomNumber OR RTRIM(rm.Description) LIKE '%' + @RoomDescription + '%' OR ISNULL(rm.RoomCode, '') LIKE '%' + @RoomDescription + '%' OR rmd.RoomDisplay LIKE '%' + @RoomDescription + '%'
                    THEN 1
                    ELSE 0
                END
        END [ContainsFilteredData]
    FROM
        #FloorplanData f
        INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        OUTER APPLY
        (
            SELECT dbo.FloorNumberShort(f.FloorNumber) + '0/' + ISNULL(NULLIF(rm.RoomCode, ''), CONVERT(VARCHAR(20), rm.Number)) + ' - ' + RTRIM(rm.Description) [RoomDisplay]
        ) rmd -- Room Display
    WHERE
        (@RoomID IS NULL OR rm.RoomID IN (SELECT RoomID FROM @RoomIDs))

    -- Get Sample data up front to reduce the main SELECT table scans.
    CREATE TABLE #SampleData (RoomID INT, SampleID INT, SampleRef VARCHAR(50), AsSample BIT NOT NULL, RegisterItemNo INT, PhotoID INT, GUID VARCHAR(50), GUIDVersion INT, SourceDescription VARCHAR(MAX), AsbestosType VARCHAR(MAX), ProductDescription VARCHAR(100), MAScore INT, PAScore INT, RiskScore INT, RiskScoreSortOrder INT, RiskScoreGroup VARCHAR(100), RiskScoreGroupID INT, RiskScoreGroupColour VARCHAR(10), RecommendedAction VARCHAR(100), RecommendedActionColour VARCHAR(10), IsMAOnly BIT, Quantity VARCHAR(MAX), Comments VARCHAR(MAX), SampleResultValue INT, Removed BIT, DateOfNextReviewInt INT, DateOfNextReviewIsAsRequired BIT NOT NULL, DocumentCount INT, ContainsFilteredData BIT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_SampleData_ID ON #SampleData (RoomID, SampleID, ContainsFilteredData)
    CREATE INDEX tempIdx_SampleData_GUID ON #SampleData (GUID, GUIDVersion DESC)

    INSERT INTO #SampleData (RoomID, SampleID, SampleRef, AsSample, RegisterItemNo, PhotoID, GUID, GUIDVersion, SourceDescription, AsbestosType, ProductDescription, MAScore, PAScore, RiskScore, RiskScoreSortOrder, RiskScoreGroup, RiskScoreGroupID, RiskScoreGroupColour, RecommendedAction, RecommendedActionColour, IsMAOnly, Quantity, Comments, SampleResultValue, Removed, DateOfNextReviewInt, DateOfNextReviewIsAsRequired, DocumentCount, ContainsFilteredData)
    SELECT DISTINCT
        rm.RoomID,
        s.SampleID,
        s.SampleRef,
        s.AsSample,
        s.RegisterItemNo,
        s.PhotoID,
        s.GUID,
        s.GUIDVersion,
        ssd.SourceDescription,
        scd.AsbestosType,
        scd.Classification [ProductDescription],
        ISNULL(scd.MaterialAssessmentScore, 0) [MAScore],
        ISNULL(scd.PriorityAssessmentScore, 0) [PAScore],
        scd.RiskScore,
        scd.RiskScoreSortOrder,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        ISNULL(scd.IsMAOnly, 0) [IsMAOnly],
        scd.Quantity,
        scd.Comments,
        scd.SampleResult [SampleResultValue],
        ISNULL(scd.Removed, 0) [Removed],
        CASE WHEN ISNUMERIC(eim27.ShortDescription) = 1 THEN CAST(eim27.ShortDescription AS INT) END [DateOfNextReviewInt],
		CASE WHEN eim27.ShortDescription = 'As Required' THEN 1 ELSE 0 END [DateOfNextReviewIsAsRequired],
        (
            SELECT COUNT(*)
            FROM SampleDocument _sd WITH (NOLOCK)
            WHERE
                _sd.SampleID = s.SampleID
                    AND
                _sd.Deleted IS NULL
        ) [DocumentCount],
        CASE WHEN @RoomNumber IS NULL AND @RoomDescription IS NULL
            THEN 1
            ELSE
                CASE WHEN s.RegisterItemNo = @RoomNumber OR ISNULL(s.SampleRef, '') LIKE '%' + @RoomDescription + '%' OR scd.SourceDescription LIKE '%' + @RoomDescription + '%' OR ISNULL(e1.ElementText, '') LIKE '%' + @RoomDescription + '%' OR ssd.SourceDescription LIKE '%' + @RoomDescription + '%'
                    THEN 1
                    ELSE 0
                END
        END [ContainsFilteredData]
    FROM
        #RoomData rm
        INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON s.SampleID = scd.SampleID
        INNER JOIN GuidSamples gs WITH (NOLOCK) ON s.SampleID = gs.SampleID AND scd.ClientID = gs.ClientID AND scd.SiteID = gs.SiteID
        LEFT JOIN Element e1 WITH (NOLOCK) ON scd.SampleID = e1.SampleID AND e1.ElementTypeID = 1
        OUTER APPLY
        (
            SELECT ISNULL(RTRIM(scd.SourceDescription), '') + ISNULL(' - ' + NULLIF(LTRIM(e1.ElementText), ''), '') [SourceDescription]
        ) ssd -- Sample Source Description
        LEFT JOIN Element e27 WITH (NOLOCK) ON scd.SampleID = e27.SampleID AND e27.ElementTypeID = 27
        LEFT JOIN ElementIntMeaning eim27 WITH (NOLOCK) ON e27.ElementIntMeaningID = eim27.ElementIntMeaningID
    WHERE
        s.Archived = 0
            AND
        (@SampleID IS NULL OR s.SampleID = @SampleID)
            AND
        CASE WHEN @RiskID IS NULL -- Risk Filter.
            THEN 1
            ELSE
                CASE WHEN scd.RiskScoreGroupID = @RiskID
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @RecAction IS NULL -- Recommended Action Filter.
            THEN 1
            ELSE
                CASE WHEN REPLACE(REPLACE(REPLACE(ISNULL(scd.RecommendedAction, ''), ' ', '_'), '&', ''), ',', '') = @RecAction
                    THEN 1
                    ELSE 0
                END
        END = 1
    GROUP BY
        rm.RoomID,
        s.SampleID,
        s.SampleRef,
        s.AsSample,
        s.RegisterItemNo,
        s.PhotoID,
        s.GUID,
        s.GUIDVersion,
        scd.SourceDescription,
        e1.ElementText,
        ssd.SourceDescription,
        scd.AsbestosType,
        scd.Classification,
        scd.MaterialAssessmentScore,
        scd.PriorityAssessmentScore,
        scd.RiskScore,
        scd.RiskScoreSortOrder,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        scd.IsMAOnly,
        scd.Quantity,
        scd.Comments,
        scd.SampleResult,
        scd.Removed,
        eim27.ShortDescription

    -- If we have a @UniqueItemCode, we need to return the treeview ID for this unique item code.
    DECLARE @QRCodeDataID INT, @QRCodeTypeID INT, @QRCodeItemGUID VARCHAR(50), @TreeviewID VARCHAR(50)
    IF @UniqueItemCode IS NOT NULL
    BEGIN
        -- Get data from the QRCode table. We need the DataID of the QR Code, and it's type.
        SELECT
            @QRCodeDataID = qrc.DataID,
            @QRCodeTypeID = qrct.QRCodeTypeID
        FROM
            QRCode qrc WITH (NOLOCK)
            INNER JOIN QRCodeType qrct WITH (NOLOCK) ON qrc.QRCodeTypeID = qrct.QRCodeTypeID
        WHERE
            qrc.UniqueCode = @UniqueItemCode

        -- Check that we have a QRCodeDataID.
        IF NULLIF(@QRCodeDataID, 0) IS NULL
        BEGIN
            SET NOEXEC ON;
        END

        -- Depending on the QRCodeTypeID, work out the treeview ID. To do this, we need to get the GUID for the QR Code item, and then get the latest item with the GUID.
        IF @QRCodeTypeID = 1
        BEGIN -- Register
            SELECT @QRCodeItemGUID = ISNULL((SELECT GUID FROM #RegisterData WHERE RegisterID = @QRCodeDataID), (SELECT GUID FROM Register WITH (NOLOCK) WHERE RegisterID = @QRCodeDataID))
            IF @QRCodeItemGUID IS NULL -- If no GUID was found, the treeview ID is the RegisterID.
            BEGIN
                SELECT @TreeviewID = 'REG' + CAST(@QRCodeDataID AS VARCHAR(50))
                SET NOEXEC ON;
            END
            SELECT TOP 1 @TreeviewID = CAST(RegisterID AS VARCHAR(50)) FROM #RegisterData WHERE GUID = @QRCodeItemGUID ORDER BY GUIDVersion DESC
            SELECT @TreeviewID = 'REG' + CASE WHEN NULLIF(@TreeviewID, '0') IS NULL THEN CAST(@QRCodeDataID AS VARCHAR(50)) ELSE @TreeviewID END
        END
        ELSE IF @QRCodeTypeID = 2
        BEGIN -- Floorplan
            SELECT @QRCodeItemGUID = ISNULL((SELECT GUID FROM #FloorplanData WHERE FloorplanID = @QRCodeDataID), (SELECT GUID FROM Floorplan WITH (NOLOCK) WHERE FloorplanID = @QRCodeDataID))
            IF @QRCodeItemGUID IS NULL -- If no GUID was found, the treeview ID is the FloorplanID.
            BEGIN
                SELECT @TreeviewID = 'F' + CAST(@QRCodeDataID AS VARCHAR(50))
                SET NOEXEC ON;
            END
            SELECT TOP 1 @TreeviewID = CAST(FloorplanID AS VARCHAR(50)) FROM #FloorplanData WHERE GUID = @QRCodeItemGUID ORDER BY GUIDVersion DESC
            SELECT @TreeviewID = 'F' + CASE WHEN NULLIF(@TreeviewID, '0') IS NULL THEN CAST(@QRCodeDataID AS VARCHAR(50)) ELSE @TreeviewID END
        END
        ELSE IF @QRCodeTypeID = 3
        BEGIN -- Room
            SELECT @QRCodeItemGUID = ISNULL((SELECT GUID FROM #RoomData WHERE RoomID = @QRCodeDataID), (SELECT GUID FROM Room WITH (NOLOCK) WHERE RoomID = @QRCodeDataID))
            IF @QRCodeItemGUID IS NULL -- If no GUID was found, the treeview ID is the RoomID.
            BEGIN
                SELECT @TreeviewID = 'RM' + CAST(@QRCodeDataID AS VARCHAR(50))
                SET NOEXEC ON;
            END
            SELECT TOP 1 @TreeviewID = CAST(RoomID AS VARCHAR(50)) FROM #RoomData WHERE GUID = @QRCodeItemGUID ORDER BY GUIDVersion DESC
            SELECT @TreeviewID = 'RM' + CASE WHEN NULLIF(@TreeviewID, '0') IS NULL THEN CAST(@QRCodeDataID AS VARCHAR(50)) ELSE @TreeviewID END
        END
        ELSE IF @QRCodeTypeID = 4
        BEGIN -- Sample
            SELECT @QRCodeItemGUID = ISNULL((SELECT GUID FROM #SampleData WHERE SampleID = @QRCodeDataID), (SELECT GUID FROM Sample WITH (NOLOCK) WHERE SampleID = @QRCodeDataID))
            IF @QRCodeItemGUID IS NULL -- If no GUID was found, the treeview ID is the SampleID.
            BEGIN
                SELECT @TreeviewID = CAST(@QRCodeDataID AS VARCHAR(50))
                SET NOEXEC ON;
            END
            SELECT TOP 1 @TreeviewID = CAST(SampleID AS VARCHAR(50)) FROM #SampleData WHERE GUID = @QRCodeItemGUID ORDER BY GUIDVersion DESC
            SELECT @TreeviewID = CASE WHEN NULLIF(@TreeviewID, '0') IS NULL THEN CAST(@QRCodeDataID AS VARCHAR(50)) ELSE @TreeviewID END
        END
    END
    SET NOEXEC OFF;

    -- Get data from PortalUserNotes in a table variable to increase speed below, if we need it.
    DECLARE @PortalUserNotesData TABLE (SampleID INT PRIMARY KEY NOT NULL, DateCreated DATETIME)
    IF @GetLastNoteCreated = 1
    BEGIN
        INSERT INTO @PortalUserNotesData (SampleID, DateCreated)
        SELECT
            s.SampleID,
            MAX(pun.DateCreated) [DateCreated]
        FROM
            (
                SELECT SampleID
                FROM #SampleData
                GROUP BY SampleID
            ) s
            INNER JOIN PortalUserNotes pun WITH (NOLOCK) ON s.SampleID = pun.ItemID AND pun.NoteType = 'Sample'
        GROUP BY
            s.SampleID
    END

    -- Declare variables for working out if we need to worry about GUID merging when querying (big performance hit, so don't if we don't need to).
    DECLARE @RegisterMergeCount INT, @FloorplanMergeCount INT, @RoomMergeCount INT, @SampleMergeCount INT
    SELECT
        @RegisterMergeCount = (SELECT COUNT(*) FROM #RegisterData WHERE GuidVersion > 1),
        @FloorplanMergeCount = (SELECT COUNT(*) FROM #FloorplanData WHERE GuidVersion > 1),
        @RoomMergeCount = (SELECT COUNT(*) FROM #RoomData WHERE GuidVersion > 1),
        @SampleMergeCount = (SELECT COUNT(*) FROM #SampleData WHERE GuidVersion > 1)

    -- Do GUID merging when querying?
    IF (@RegisterMergeCount + @FloorplanMergeCount + @RoomMergeCount + @SampleMergeCount) > 0
    BEGIN
        DECLARE @SampleCollection TABLE (ClientId INT, JobID INT, JobNo INT, SiteID INT, Post2000 BIT, Employee VARCHAR(MAX), RegisterID INT, BuildingDesignation VARCHAR(MAX), SurveyStartDate DATETIME, SurveyFinishDate DATETIME, SurveyType VARCHAR(100), FloorplanID INT, FloorNumber INT, FloorDescription VARCHAR(MAX), FloorplanIDWithData INT, RoomID INT, RoomNumber INT, Room VARCHAR(MAX), SampleID INT, RegisterItemNo INT, SampleRef VARCHAR(MAX), SampleRefUnformatted VARCHAR(MAX), AsSample BIT, SourceDescription VARCHAR(MAX), AsbestosType VARCHAR(MAX), ProductDescription VARCHAR(MAX), MAScore INT, PAScore INT, RiskScore INT, RiskScoreSortOrder INT, RiskScoreGroup VARCHAR(MAX), RiskScoreGroupID INT, RiskScoreGroupColour VARCHAR(MAX), RecommendedAction VARCHAR(MAX), RecommendedActionColour VARCHAR(MAX), IsMAOnly BIT, Quantity VARCHAR(MAX), Comments VARCHAR(MAX), SampleResultValue INT, Removed BIT, ReinspectionDate DATETIME, PhotoID INT, DocumentCount INT NULL)

        -- Get the main data, where there is data at Sample level.
        INSERT INTO @SampleCollection (ClientId, JobID, JobNo, SiteID, Post2000, Employee, RegisterID, BuildingDesignation, SurveyStartDate, SurveyFinishDate, SurveyType, FloorplanID, FloorNumber, FloorDescription, FloorplanIDWithData, RoomID, RoomNumber, Room, SampleID, RegisterItemNo, SampleRef, SampleRefUnformatted, AsSample, SourceDescription, AsbestosType, ProductDescription, MAScore, PAScore, RiskScore, RiskScoreSortOrder, RiskScoreGroup, RiskScoreGroupID, RiskScoreGroupColour, RecommendedAction, RecommendedActionColour, IsMAOnly, Quantity, Comments, SampleResultValue, Removed, ReinspectionDate, PhotoID, DocumentCount)
        SELECT
            j.ClientID,
            ISNULL(rgm.JobID, r.JobID) [JobID],
            ISNULL(rgm.JobNo, r.JobNo) [JobNo],
            j.SiteID,
            si.Post2000,
            e.FullName [Employee],
            ISNULL(rgm.RegisterID, r.RegisterID) [RegisterID],
            COALESCE(rgm.BuildingDesignation, r.BuildingDesignation, 'No Building Designation') [BuildingDesignation],
            ISNULL(rgm.SurveyStartDate, r.SurveyStartDate) [SurveyStartDate],
            ISNULL(rgm.SurveyFinishDate, r.SurveyFinishDate) [SurveyFinishDate],
            ISNULL(rgm.SurveyType, r.SurveyType) [SurveyType],
            ISNULL(fgm.FloorplanID, f.FloorplanID) [FloorplanID],
            ISNULL(fgm.FloorNumber, f.FloorNumber) [FloorNumber],
            ISNULL(fgm.Description, f.Description) [FloorDescription],
            CASE WHEN fgmwd.FloorplanIDWithData IS NOT NULL THEN fgmwd.FloorplanIDWithData ELSE CASE WHEN f.HasFloorplan = 1 THEN f.FloorplanID ELSE NULL END END [FloorplanIDWithData],
            ISNULL(rmgm.RoomID, rm.RoomID) [RoomID],
            ISNULL(rmgm.Number, rm.Number) [RoomNumber],
            ISNULL(rmgm.RoomDisplay, rm.RoomDisplay) [Room],
            ISNULL(sgm.SampleID, s.SampleID) [SampleID],
            ISNULL(sgm.RegisterItemNo, s.RegisterItemNo) [RegisterItemNo],
            dbo.FormatPortalSampleRef(ISNULL(sgm.SampleRef, s.SampleRef)) [SampleRef],
            ISNULL(sgm.SampleRef, s.SampleRef) [SampleRefUnformatted],
            ISNULL(sgm.AsSample, s.AsSample) [AsSample],
            ISNULL(sgm.SourceDescription, s.SourceDescription) [SourceDescription],
            ISNULL(sgm.AsbestosType, s.AsbestosType) [AsbestosType],
            ISNULL(sgm.ProductDescription, s.ProductDescription) [ProductDescription],
            ISNULL(sgm.MAScore, s.MAScore) [MAScore],
            ISNULL(sgm.PAScore, s.PAScore) [PAScore],
            ISNULL(sgm.RiskScore, s.RiskScore) [RiskScore],
            ISNULL(sgm.RiskScoreSortOrder, s.RiskScoreSortOrder) [RiskScoreSortOrder],
            ISNULL(sgm.RiskScoreGroup, s.RiskScoreGroup) [RiskScoreGroup],
            ISNULL(sgm.RiskScoreGroupID, s.RiskScoreGroupID) [RiskScoreGroupID],
            ISNULL(sgm.RiskScoreGroupColour, s.RiskScoreGroupColour) [RiskScoreGroupColour],
            ISNULL(sgm.RecommendedAction, s.RecommendedAction) [RecommendedAction],
            ISNULL(sgm.RecommendedActionColour, s.RecommendedActionColour) [RecommendedActionColour],
            ISNULL(sgm.IsMAOnly, s.IsMAOnly) [IsMAOnly],
            ISNULL(sgm.Quantity, s.Quantity) [Quantity],
            ISNULL(sgm.Comments, s.Comments) [Comments],
            ISNULL(sgm.SampleResultValue, s.SampleResultValue) [SampleResultValue],
            ISNULL(sgm.Removed, s.Removed) [Removed],
            CASE
                WHEN ISNULL(sgm.DateOfNextReviewInt, s.DateOfNextReviewInt) IS NOT NULL THEN DATEADD(MONTH, ISNULL(sgm.DateOfNextReviewInt, s.DateOfNextReviewInt), ISNULL(rgm.SurveyFinishDate, r.SurveyFinishDate))
				WHEN ISNULL(sgm.DateOfNextReviewIsAsRequired, s.DateOfNextReviewIsAsRequired) = 1 THEN NULL
                WHEN ISNULL(sgm.RiskScoreGroupID, s.RiskScoreGroupID) = 4 THEN DATEADD(MONTH, 6, ISNULL(rgm.SurveyFinishDate, r.SurveyFinishDate))
                WHEN ISNULL(sgm.RiskScoreGroupID, s.RiskScoreGroupID) IS NOT NULL THEN DATEADD(MONTH, 12, ISNULL(rgm.SurveyFinishDate, r.SurveyFinishDate))
            END [ReinspectionDate],
            ISNULL(sgm.PhotoID, s.PhotoID) [PhotoID],
            ISNULL(sgm.DocumentCount, s.DocumentCount) [DocumentCount]
        FROM
            #SampleData s
            OUTER APPLY
            (
                SELECT TOP 1
                    sgm.SampleID,
                    sgm.RegisterItemNo,
                    sgm.SampleRef,
                    sgm.AsSample,
                    sgm.PhotoID,
                    sgm.SourceDescription,
                    sgm.AsbestosType,
                    sgm.ProductDescription,
                    sgm.MAScore,
                    sgm.PAScore,
                    sgm.RiskScore,
                    sgm.RiskScoreSortOrder,
                    sgm.RiskScoreGroup,
                    sgm.RiskScoreGroupID,
                    sgm.RiskScoreGroupColour,
                    sgm.RecommendedAction,
                    sgm.RecommendedActionColour,
                    sgm.IsMAOnly,
                    sgm.Quantity,
                    sgm.Comments,
                    sgm.SampleResultValue,
                    sgm.Removed,
                    sgm.DateOfNextReviewInt,
                    sgm.DateOfNextReviewIsAsRequired,
                    sgm.DocumentCount
                FROM
                    #SampleData sgm
                WHERE
                    sgm.GUID = s.GUID
                ORDER BY
                    sgm.GUIDVersion DESC
            ) sgm
            INNER JOIN #RoomData rm ON s.RoomID = rm.RoomID AND ( -- Only show Rooms where a filter matches, or all Rooms that contain a Sample with a filter match.
                rm.ContainsFilteredData = 1
                    OR
                EXISTS(SELECT 1 FROM #SampleData WHERE RoomID = rm.RoomID AND ContainsFilteredData = 1)
            )
            OUTER APPLY
            (
                SELECT TOP 1
                    rmgm.RoomID,
                    rmgm.Description,
                    rmgm.Number,
                    rmgm.RoomCode,
                    rmgm.RoomDisplay
                FROM
                    #RoomData rmgm
                WHERE
                    rmgm.GUID = rm.GUID
                ORDER BY
                    rmgm.GUIDVersion DESC
            ) rmgm
            INNER JOIN #FloorplanData f ON rm.FloorplanID = f.FloorplanID
            OUTER APPLY
            (
                SELECT TOP 1
                    fgm.FloorplanID,
                    fgm.FloorNumber,
                    fgm.Description,
                    fgm.HasFloorplan
                FROM
                    #FloorplanData fgm
                WHERE
                    fgm.GUID = f.GUID
                ORDER BY
                    fgm.GUIDVersion DESC
            ) fgm
            OUTER APPLY
            (
                SELECT TOP 1 fgmwd.FloorplanID [FloorplanIDWithData]
                FROM
                    #FloorplanData fgmwd
                WHERE
                    fgmwd.GUID = f.GUID
                        AND
                    fgmwd.HasFloorplan = 1
                ORDER BY
                    fgmwd.GUIDVersion DESC
            ) fgmwd
            INNER JOIN #RegisterData r ON f.RegisterID = r.RegisterID
            OUTER APPLY
            (
                SELECT TOP 1
                    rgm.JobID,
                    rgm.JobNo,
                    rgm.RegisterID,
                    rgm.BuildingDesignation,
                    rgm.EmployeeID,
                    rgm.SurveyStartDate,
                    rgm.SurveyFinishDate,
                    rgm.SurveyType
                FROM
                    #RegisterData rgm
                WHERE
                    rgm.GUID = r.GUID
                ORDER BY
                    rgm.GUIDVersion DESC
            ) rgm
            INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
            INNER JOIN Employee e WITH (NOLOCK) ON ISNULL(rgm.EmployeeID, je.EmployeeID) = e.EmployeeID
            INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            INNER JOIN @ClientData c ON j.ClientID = c.ClientID
        WHERE -- Only show Samples where a filter matches, or all Samples in a Room where a Room filter matches.
            s.ContainsFilteredData = 1
                OR
            EXISTS(SELECT 1 FROM #RoomData WHERE RoomID = s.RoomID AND ContainsFilteredData = 1)

        -- Get the other data, where there isn't data at Sample level but it goes up to Room level.
        INSERT INTO @SampleCollection (ClientId, JobID, JobNo, SiteID, Post2000, Employee, RegisterID, BuildingDesignation, SurveyStartDate, SurveyFinishDate, SurveyType, FloorplanID, FloorNumber, FloorDescription, FloorplanIDWithData, RoomID, RoomNumber, Room, SampleID, RegisterItemNo, SampleRef, SampleRefUnformatted, AsSample, SourceDescription, AsbestosType, ProductDescription, MAScore, PAScore, RiskScore, RiskScoreSortOrder, RiskScoreGroup, RiskScoreGroupID, RiskScoreGroupColour, RecommendedAction, RecommendedActionColour, IsMAOnly, Quantity, Comments, SampleResultValue, Removed, ReinspectionDate, PhotoID, DocumentCount)
        SELECT
            j.ClientID,
            ISNULL(rgm.JobID, r.JobID) [JobID],
            ISNULL(rgm.JobNo, r.JobNo) [JobNo],
            j.SiteID,
            si.Post2000,
            e.FullName [Employee],
            ISNULL(rgm.RegisterID, r.RegisterID) [RegisterID],
            COALESCE(rgm.BuildingDesignation, r.BuildingDesignation, 'No Building Designation') [BuildingDesignation],
            ISNULL(rgm.SurveyStartDate, r.SurveyStartDate) [SurveyStartDate],
            ISNULL(rgm.SurveyFinishDate, r.SurveyFinishDate) [SurveyFinishDate],
            ISNULL(rgm.SurveyType, r.SurveyType) [SurveyType],
            ISNULL(fgm.FloorplanID, f.FloorplanID) [FloorplanID],
            ISNULL(fgm.FloorNumber, f.FloorNumber) [FloorNumber],
            ISNULL(fgm.Description, f.Description) [FloorDescription],
            CASE WHEN fgmwd.FloorplanIDWithData IS NOT NULL THEN fgmwd.FloorplanIDWithData ELSE CASE WHEN f.HasFloorplan = 1 THEN f.FloorplanID ELSE NULL END END [FloorplanIDWithData],
            ISNULL(rmgm.RoomID, rm.RoomID) [RoomID],
            ISNULL(rmgm.Number, rm.Number) [RoomNumber],
            ISNULL(rmgm.RoomDisplay, rm.RoomDisplay) [Room],
            NULL [SampleID],
            NULL [RegisterItemNo],
            NULL [SampleRef],
            NULL [SampleRefUnformatted],
            NULL [AsSample],
            NULL [SourceDescription],
            NULL [AsbestosType],
            NULL [ProductDescription],
            NULL [MAScore],
            NULL [PAScore],
            NULL [RiskScore],
            NULL [RiskScoreSortOrder],
            NULL [RiskScoreGroup],
            NULL [RiskScoreGroupID],
            NULL [RiskScoreGroupColour],
            NULL [RecommendedAction],
            NULL [RecommendedActionColour],
            NULL [IsMAOnly],
            NULL [Quantity],
            NULL [Comments],
            NULL [SampleResultValue],
            NULL [Removed],
            NULL [ReinspectionDate],
            NULL [PhotoID],
            NULL [DocumentCount]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN @ClientData c ON j.ClientID = c.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN #RegisterData r ON je.JobEmployeeID = r.JobEmployeeID
            OUTER APPLY
            (
                SELECT TOP 1
                    rgm.JobID,
                    rgm.JobNo,
                    rgm.RegisterID,
                    rgm.BuildingDesignation,
                    rgm.EmployeeID,
                    rgm.SurveyStartDate,
                    rgm.SurveyFinishDate,
                    rgm.SurveyType
                FROM
                    #RegisterData rgm
                WHERE
                    rgm.GUID = r.GUID
                ORDER BY
                    rgm.GUIDVersion DESC
            ) rgm
            INNER JOIN Employee e WITH (NOLOCK) ON ISNULL(rgm.EmployeeID, je.EmployeeID) = e.EmployeeID
            LEFT JOIN #FloorplanData f ON r.RegisterID = f.RegisterID
            OUTER APPLY
            (
                SELECT TOP 1
                    fgm.FloorplanID,
                    fgm.FloorNumber,
                    fgm.Description,
                    fgm.HasFloorplan
                FROM
                    #FloorplanData fgm
                WHERE
                    fgm.GUID = f.GUID
                ORDER BY
                    fgm.GUIDVersion DESC
            ) fgm
            OUTER APPLY
            (
                SELECT TOP 1 fgmwd.FloorplanID [FloorplanIDWithData]
                FROM
                    #FloorplanData fgmwd
                WHERE
                    fgmwd.GUID = f.GUID
                        AND
                    fgmwd.HasFloorplan = 1
                ORDER BY
                    fgmwd.GUIDVersion DESC
            ) fgmwd
            LEFT JOIN #RoomData rm ON f.FloorplanID = rm.FloorplanID AND ( -- Only show Rooms where a filter matches, or all Rooms that contain a Sample with a filter match.
                rm.ContainsFilteredData = 1
                    OR
                EXISTS(SELECT 1 FROM #SampleData WHERE RoomID = rm.RoomID AND ContainsFilteredData = 1)
            )
            OUTER APPLY
            (
                SELECT TOP 1
                    rmgm.RoomID,
                    rmgm.Description,
                    rmgm.Number,
                    rmgm.RoomCode,
                    rmgm.RoomDisplay
                FROM
                    #RoomData rmgm
                WHERE
                    rmgm.GUID = rm.GUID
                ORDER BY
                    rmgm.GUIDVersion DESC
            ) rmgm
        WHERE -- Don't get data if we have it already.
            (ISNULL(rmgm.RoomID, rm.RoomID) NOT IN (SELECT RoomID FROM @SampleCollection GROUP BY RoomID))
                OR
            (
                ISNULL(rmgm.RoomID, rm.RoomID) IS NULL
                    AND
                (ISNULL(fgm.FloorplanID, f.FloorplanID) NOT IN (SELECT FloorplanID FROM @SampleCollection GROUP BY FloorplanID))
            )
                OR
            (
                ISNULL(fgm.FloorplanID, f.FloorplanID) IS NULL
                    AND
                (ISNULL(rgm.RegisterID, r.RegisterID) NOT IN (SELECT RegisterID FROM @SampleCollection GROUP BY RegisterID))
            )

        -- Start the main SELECT.
        SELECT DISTINCT
            sc.*,
            CASE WHEN @GetLastNoteCreated = 1
                THEN (SELECT DateCreated FROM @PortalUserNotesData WHERE SampleID = sc.SampleID)
                ELSE NULL
            END [LastNoteCreated],
            @TreeviewID [TreeviewID]
        FROM
            @SampleCollection sc
        ORDER BY
            sc.RecommendedAction,
            sc.SampleRef,
            sc.RoomID
    END
    ELSE
    BEGIN -- Start the main SELECT. Ignore Merged Items as these are not needed.
        SELECT DISTINCT
            j.ClientID,
            r.JobID,
            r.JobNo,
            j.SiteID,
            si.Post2000,
            e.FullName [Employee],
            r.RegisterID,
            ISNULL(r.BuildingDesignation, 'No Building Designation') [BuildingDesignation],
            r.SurveyStartDate,
            r.SurveyFinishDate,
            r.SurveyType,
            f.FloorplanID,
            f.FloorNumber,
            f.Description [FloorDescription],
            CASE WHEN f.HasFloorplan = 1 THEN f.FloorplanID ELSE NULL END [FloorplanIDWithData],
            rm.RoomID,
            rm.Number [RoomNumber],
            rm.RoomDisplay [Room],
            s.SampleID,
            s.RegisterItemNo,
            dbo.FormatPortalSampleRef(s.SampleRef) [SampleRef],
            s.SampleRef [SampleRefUnformatted],
            s.AsSample,
            s.SourceDescription,
            s.AsbestosType,
            s.ProductDescription,
            s.MAScore,
            s.PAScore,
            s.RiskScore,
            s.RiskScoreSortOrder,
            s.RiskScoreGroup,
            s.RiskScoreGroupID,
            s.RiskScoreGroupColour,
            s.RecommendedAction,
            s.RecommendedActionColour,
            s.IsMAOnly,
            s.Quantity,
            s.Comments,
            s.SampleResultValue,
            s.Removed,
            CASE
                WHEN s.DateOfNextReviewInt IS NOT NULL THEN DATEADD(MONTH, s.DateOfNextReviewInt, r.SurveyFinishDate)
				WHEN s.DateOfNextReviewIsAsRequired = 1 THEN NULL
                WHEN s.RiskScoreGroupID = 4 THEN DATEADD(MONTH, 6, r.SurveyFinishDate)
                WHEN s.RiskScoreGroupID IS NOT NULL THEN DATEADD(MONTH, 12, r.SurveyFinishDate)
            END [ReinspectionDate],
            s.PhotoID,
            s.DocumentCount,
            CASE WHEN @GetLastNoteCreated = 1
                THEN (SELECT DateCreated FROM @PortalUserNotesData WHERE SampleID = s.SampleID)
                ELSE NULL
            END [LastNoteCreated],
            @TreeviewID [TreeviewID]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN @ClientData c ON j.ClientID = c.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
            INNER JOIN #RegisterData r ON je.JobEmployeeID = r.JobEmployeeID
            LEFT JOIN #FloorplanData f ON f.RegisterID = r.RegisterID
            LEFT JOIN #RoomData rm ON f.FloorplanID = rm.FloorplanID AND ( -- Only show Rooms where a filter matches, or all Rooms that contain a Sample with a filter match.
                rm.ContainsFilteredData = 1
                    OR
                EXISTS(SELECT 1 FROM #SampleData WHERE RoomID = rm.RoomID AND ContainsFilteredData = 1)
            )
            LEFT JOIN #SampleData s ON rm.RoomID = s.RoomID AND ( -- Only show Samples where a filter matches, or all Samples in a Room where a Room filter matches.
                s.ContainsFilteredData = 1
                    OR
                EXISTS(SELECT 1 FROM #RoomData WHERE RoomID = s.RoomID AND ContainsFilteredData = 1)
            )
        ORDER BY
            s.RecommendedAction,
            s.SampleRef
    END

    -- Clear up temp tables.
    DROP TABLE #RegisterData
    DROP TABLE #FloorplanData
    DROP TABLE #RoomData
    DROP TABLE #SampleData


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetSamples_Survey]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX),
    @JobID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @RegisterID INT = NULL,
    @FloorplanID INT = NULL,
    @RoomID INT = NULL,
    @SampleID INT = NULL,
    @RoomDescription VARCHAR(100) = '', -- This is a Description filter, not just for Rooms but Sample Ref's, Descriptions, etc.
    @RoomNumber INT = NULL, -- This is a Number filter, not just for Rooms but Register Item Numbers as well.
    @GetLastNoteCreated BIT = NULL, -- Whether or not we want to get when the last note was created. This is a performance hit, so only get it when we need it.
    @UniqueItemCode VARCHAR(20) = NULL, -- A unique item code, passed in via QR Codes. From this, we return a treeview ID for the latest item with the same GUID, which gets selected in the treeview.
    @RiskID INT = NULL, -- Filter to Samples with this RiskID only?
    @RecAction VARCHAR(100) = NULL -- Filter to Samples with this Recommended Action only?
/**********************************************************************
** Overview: Get samples for the portal site/survey info page
** NOTE: Only used as of initial revision by the portal for computed data
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @JobID = NULLIF(@JobID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @RegisterID = NULLIF(@RegisterID, 0),
        @FloorplanID = NULLIF(@FloorplanID, 0),
        @RoomID = NULLIF(@RoomID, 0),
        @SampleID = NULLIF(@SampleID, 0),
        @RoomDescription = NULLIF(LTRIM(RTRIM(@RoomDescription)), ''),
        @RoomNumber = NULLIF(@RoomNumber, 0),
        @GetLastNoteCreated = ISNULL(@GetLastNoteCreated, 1),
        @UniqueItemCode = NULLIF(@UniqueItemCode, ''),
        @RiskID = NULLIF(@RiskID, 0),
        @RecAction = NULLIF(REPLACE(REPLACE(REPLACE(ISNULL(@RecAction, ''), ' ', '_'), '&', ''), ',', ''), '')

    -- Set IDs lower down the tree if IDs higher up the tree are set.
    IF @SampleID IS NOT NULL AND @RoomID IS NULL
    BEGIN
        SELECT @RoomID = RoomID FROM Sample WITH (NOLOCK) WHERE SampleID = @SampleID
    END
    IF @RoomID IS NOT NULL AND @FloorplanID IS NULL
    BEGIN
        SELECT @FloorplanID = FloorplanID FROM Room WITH (NOLOCK) WHERE RoomID = @RoomID
    END
    IF @FloorplanID IS NOT NULL AND @RegisterID IS NULL
    BEGIN
        SELECT @RegisterID = RegisterID FROM Floorplan WITH (NOLOCK) WHERE FloorplanID = @FloorplanID
    END
    IF (@JobID IS NULL AND @SiteIDs IS NULL AND @RegisterID IS NULL AND @FloorplanID IS NULL AND @RoomID IS NULL AND @SampleID IS NULL)
    BEGIN
        RAISERROR ('No params passed to GetSamples.', 16, 1)
        RETURN;
    END

    -- Get all Clients into a table variable.
    DECLARE @ClientData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientData (ClientID)
    SELECT s
    FROM dbo.SplitString(@ClientIDs, ',')

    -- Get all Sites into a table variable.
    DECLARE @SiteData TABLE (SiteID INT PRIMARY KEY)
    IF @JobID IS NULL
    BEGIN
        INSERT INTO @SiteData (SiteID)
        SELECT s
        FROM dbo.SplitString(@SiteIDs, ',')
    END

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @BasementName VARCHAR(50)
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @BasementName = ISNULL(NULLIF(cfg.s__BasementName, ''), 'Z-Sub Level')
    FROM
        Config cfg WITH (NOLOCK)

    -- Get Register data up front to reduce the main SELECT table scans.
    CREATE TABLE #RegisterData (JobID INT, JobNo INT, JobEmployeeID INT, EmployeeID INT, RegisterID INT, BuildingDesignation VARCHAR(1000), GUID VARCHAR(50), GUIDVersion INT, SurveyStartDate DATETIME, SurveyFinishDate DATETIME, SurveyType VARCHAR(100))

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_RegisterData_ID ON #RegisterData (JobEmployeeID, RegisterID)

    INSERT INTO #RegisterData (JobID, JobNo, JobEmployeeID, EmployeeID, RegisterID, BuildingDesignation, GUID, GUIDVersion, SurveyStartDate, SurveyFinishDate, SurveyType)
    SELECT
        j.JobID,
        j.JobNo,
        je.JobEmployeeID,
        je.EmployeeID,
        r.RegisterID,
        r.BuildingDesignation,
        r.GUID,
        r.GUIDVersion,
        CAST(MIN(r.RegisterStart) AS DATE) [SurveyStartDate],
        CAST(MIN(r.RegisterFinish) AS DATE) [SurveyFinishDate],
        sut.Description [SurveyType]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientData c ON j.ClientID = c.ClientID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN SurveyType sut WITH (NOLOCK) ON su.SurveyTypeID = sut.SurveyTypeID
    WHERE
        (@JobID IS NULL OR j.JobID = @JobID)
            AND
        (@JobID IS NOT NULL OR j.SiteID IN (SELECT SiteID FROM @SiteData))
            AND
        j.Cancelled IS NULL
            AND
        j.Approved IS NOT NULL
            AND
        (@RegisterID IS NULL OR r.RegisterID = @RegisterID)
    GROUP BY
        j.JobID,
        j.JobNo,
        je.JobEmployeeID,
        je.EmployeeID,
        r.RegisterID,
        r.BuildingDesignation,
        r.GUID,
        r.GUIDVersion,
        sut.Description

    -- Get Floorplan data up front to reduce the main SELECT table scans.
    CREATE TABLE #FloorplanData (RegisterID INT, BuildingDesignation VARCHAR(1000), FloorplanID INT, FloorNumber INT, Description VARCHAR(MAX), TEAMS_StoreID INT, HasFloorplan BIT, GUID VARCHAR(50), GUIDVersion INT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_FloorplanData_ID ON #FloorplanData (RegisterID, FloorplanID)

    INSERT INTO #FloorplanData (RegisterID, BuildingDesignation, FloorplanID, FloorNumber, Description, TEAMS_StoreID, HasFloorplan, GUID, GUIDVersion)
    SELECT
        r.RegisterID,
        r.BuildingDesignation,
        f.FloorplanID,
        f.FloorNumber,
        ISNULL(f.DescriptionOverride, REPLACE(dbo.FloorName(f.FloorNumber), 'Z-Sub Level', @BasementName)) [Description],
        f.TEAMS_StoreID,
        CASE WHEN f.FloorplanData IS NOT NULL THEN 1 ELSE CASE WHEN f.AutocadData IS NOT NULL THEN 1 ELSE 0 END END [HasFloorplan],
        f.GUID,
        f.GUIDVersion
    FROM
        #RegisterData r
        INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
    WHERE
        (@FloorplanID IS NULL OR f.FloorplanID = @FloorplanID)

    -- Get Room data up front to reduce the main SELECT table scans.
    CREATE TABLE #RoomData (FloorplanID INT, RoomID INT, Description VARCHAR(200), Number INT, RoomCode VARCHAR(MAX), RoomDisplay VARCHAR(MAX), GUID VARCHAR(50), GUIDVersion INT, ContainsFilteredData BIT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_RoomData_ID ON #RoomData (FloorplanID, RoomID, ContainsFilteredData)

    INSERT INTO #RoomData (FloorplanID, RoomID, Description, Number, RoomCode, RoomDisplay, GUID, GUIDVersion, ContainsFilteredData)
    SELECT
        f.FloorplanID,
        rm.RoomID,
        RTRIM(rm.Description) [Description],
        rm.Number,
        NULLIF(rm.RoomCode, '') [RoomCode],
        rmd.RoomDisplay,
        rm.GUID,
        rm.GUIDVersion,
        CASE WHEN @RoomNumber IS NULL AND @RoomDescription IS NULL
            THEN 1
            ELSE
                CASE WHEN rm.Number = @RoomNumber OR RTRIM(rm.Description) LIKE '%' + @RoomDescription + '%' OR ISNULL(rm.RoomCode, '') LIKE '%' + @RoomDescription + '%' OR rmd.RoomDisplay LIKE '%' + @RoomDescription + '%'
                    THEN 1
                    ELSE 0
                END
        END [ContainsFilteredData]
    FROM
        #FloorplanData f
        INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        OUTER APPLY
        (
            SELECT dbo.FloorNumberShort(f.FloorNumber) + '0/' + ISNULL(NULLIF(rm.RoomCode, ''), CONVERT(VARCHAR(20), rm.Number)) + ' - ' + RTRIM(rm.Description) [RoomDisplay]
        ) rmd -- Room Display
    WHERE
        (@RoomID IS NULL OR rm.RoomID = @RoomID)

    -- Get Sample data up front to reduce the main SELECT table scans.
    CREATE TABLE #SampleData (RoomID INT, SampleID INT, SampleRef VARCHAR(50), AsSample BIT NOT NULL, RegisterItemNo INT, PhotoID INT, GUID VARCHAR(50), GUIDVersion INT, SourceDescription VARCHAR(MAX), AsbestosType VARCHAR(MAX), ProductDescription VARCHAR(100), MAScore INT, PAScore INT, RiskScore INT, RiskScoreSortOrder INT, RiskScoreGroup VARCHAR(100), RiskScoreGroupID INT, RiskScoreGroupColour VARCHAR(10), RecommendedAction VARCHAR(100), RecommendedActionColour VARCHAR(10), IsMAOnly BIT, Quantity VARCHAR(MAX), Comments VARCHAR(MAX), SampleResultValue INT, Removed BIT, DateOfNextReviewInt INT, DateOfNextReviewIsAsRequired BIT NOT NULL, DocumentCount INT, ContainsFilteredData BIT)

    -- Add an index on important fields to increase speed in the main SELECT.
    CREATE INDEX tempIdx_SampleData_ID ON #SampleData (RoomID, SampleID, ContainsFilteredData)

    INSERT INTO #SampleData (RoomID, SampleID, SampleRef, AsSample, RegisterItemNo, PhotoID, GUID, GUIDVersion, SourceDescription, AsbestosType, ProductDescription, MAScore, PAScore, RiskScore, RiskScoreSortOrder, RiskScoreGroup, RiskScoreGroupID, RiskScoreGroupColour, RecommendedAction, RecommendedActionColour, IsMAOnly, Quantity, Comments, SampleResultValue, Removed, DateOfNextReviewInt, DateOfNextReviewIsAsRequired, DocumentCount, ContainsFilteredData)
    SELECT DISTINCT
        rm.RoomID,
        s.SampleID,
        s.SampleRef,
        s.AsSample,
        s.RegisterItemNo,
        s.PhotoID,
        s.GUID,
        s.GUIDVersion,
        ssd.SourceDescription,
        scd.AsbestosType,
        scd.Classification [ProductDescription],
        ISNULL(scd.MaterialAssessmentScore, 0) [MAScore],
        ISNULL(scd.PriorityAssessmentScore, 0) [PAScore],
        scd.RiskScore,
        scd.RiskScoreSortOrder,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        ISNULL(scd.IsMAOnly, 0) [IsMAOnly],
        scd.Quantity,
        scd.Comments,
        scd.SampleResult [SampleResultValue],
        ISNULL(scd.Removed, 0) [Removed],
        CASE WHEN ISNUMERIC(eim27.ShortDescription) = 1 THEN CAST(eim27.ShortDescription AS INT) END [DateOfNextReviewInt],
		CASE WHEN eim27.ShortDescription = 'As Required' THEN 1 ELSE 0 END [DateOfNextReviewIsAsRequired],
        (
            SELECT COUNT(*)
            FROM SampleDocument _sd WITH (NOLOCK)
            WHERE
                _sd.SampleID = s.SampleID
                    AND
                _sd.Deleted IS NULL
        ) [DocumentCount],
        CASE WHEN @RoomNumber IS NULL AND @RoomDescription IS NULL
            THEN 1
            ELSE
                CASE WHEN s.RegisterItemNo = @RoomNumber OR ISNULL(s.SampleRef, '') LIKE '%' + @RoomDescription + '%' OR scd.SourceDescription LIKE '%' + @RoomDescription + '%' OR ISNULL(e1.ElementText, '') LIKE '%' + @RoomDescription + '%' OR ssd.SourceDescription LIKE '%' + @RoomDescription + '%'
                    THEN 1
                    ELSE 0
                END
        END [ContainsFilteredData]
    FROM
        #RoomData rm
        INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON s.SampleID = scd.SampleID
        LEFT JOIN Element e1 WITH (NOLOCK) ON scd.SampleID = e1.SampleID AND e1.ElementTypeID = 1
        OUTER APPLY
        (
            SELECT ISNULL(RTRIM(scd.SourceDescription), '') + ISNULL(' - ' + NULLIF(LTRIM(e1.ElementText), ''), '') [SourceDescription]
        ) ssd -- Sample Source Description
        LEFT JOIN Element e27 WITH (NOLOCK) ON scd.SampleID = e27.SampleID AND e27.ElementTypeID = 27
        LEFT JOIN ElementIntMeaning eim27 WITH (NOLOCK) ON e27.ElementIntMeaningID = eim27.ElementIntMeaningID
    WHERE
        s.Archived = 0
            AND
        (@SampleID IS NULL OR s.SampleID = @SampleID)
            AND
        CASE WHEN @RiskID IS NULL -- Risk Filter.
            THEN 1
            ELSE
                CASE WHEN scd.RiskScoreGroupID = @RiskID
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @RecAction IS NULL -- Recommended Action Filter.
            THEN 1
            ELSE
                CASE WHEN REPLACE(REPLACE(REPLACE(ISNULL(scd.RecommendedAction, ''), ' ', '_'), '&', ''), ',', '') = @RecAction
                    THEN 1
                    ELSE 0
                END
        END = 1
    GROUP BY
        rm.RoomID,
        s.SampleID,
        s.SampleRef,
        s.AsSample,
        s.RegisterItemNo,
        s.PhotoID,
        s.GUID,
        s.GUIDVersion,
        scd.SourceDescription,
        e1.ElementText,
        ssd.SourceDescription,
        scd.AsbestosType,
        scd.Classification,
        scd.MaterialAssessmentScore,
        scd.PriorityAssessmentScore,
        scd.RiskScore,
        scd.RiskScoreSortOrder,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        scd.IsMAOnly,
        scd.Quantity,
        scd.Comments,
        scd.SampleResult,
        scd.Removed,
        eim27.ShortDescription

    -- If we have a @UniqueItemCode, we need to return the treeview ID for this unique item code.
    DECLARE @QRCodeDataID INT, @QRCodeTypeID INT, @QRCodeItemGUID VARCHAR(50), @TreeviewID VARCHAR(50)
    IF @UniqueItemCode IS NOT NULL
    BEGIN
        -- Get data from the QRCode table. We need the DataID of the QR Code, and it's type.
        SELECT
            @QRCodeDataID = qrc.DataID,
            @QRCodeTypeID = qrct.QRCodeTypeID
        FROM
            QRCode qrc WITH (NOLOCK)
            INNER JOIN QRCodeType qrct WITH (NOLOCK) ON qrc.QRCodeTypeID = qrct.QRCodeTypeID
        WHERE
            qrc.UniqueCode = @UniqueItemCode

        -- Check that we have a QRCodeDataID.
        IF NULLIF(@QRCodeDataID, 0) IS NULL
        BEGIN
            SET NOEXEC ON;
        END

        -- Depending on the QRCodeTypeID, work out the treeview ID. As we are in the Surveys tab, we don't need to worry about GUIDs, so just return the ID.
        IF @QRCodeTypeID = 1
        BEGIN -- Register
            SELECT @TreeviewID = 'REG' + CAST(@QRCodeDataID AS VARCHAR(50))
        END
        ELSE IF @QRCodeTypeID = 2
        BEGIN -- Floorplan
            SELECT @TreeviewID = 'F' + CAST(@QRCodeDataID AS VARCHAR(50))
        END
        ELSE IF @QRCodeTypeID = 3
        BEGIN -- Room
            SELECT @TreeviewID = 'RM' + CAST(@QRCodeDataID AS VARCHAR(50))
        END
        ELSE IF @QRCodeTypeID = 4
        BEGIN -- Sample
            SELECT @TreeviewID = CAST(@QRCodeDataID AS VARCHAR(50))
        END
    END
    SET NOEXEC OFF;

    -- Get data from PortalUserNotes in a table variable to increase speed below, if we need it.
    DECLARE @PortalUserNotesData TABLE (SampleID INT PRIMARY KEY NOT NULL, DateCreated DATETIME)
    IF @GetLastNoteCreated = 1
    BEGIN
        INSERT INTO @PortalUserNotesData (SampleID, DateCreated)
        SELECT
            s.SampleID,
            MAX(pun.DateCreated) [DateCreated]
        FROM
            (
                SELECT SampleID
                FROM #SampleData
                GROUP BY SampleID
            ) s
            INNER JOIN PortalUserNotes pun WITH (NOLOCK) ON s.SampleID = pun.ItemID AND pun.NoteType = 'Sample'
        GROUP BY
            s.SampleID
    END

    -- Start the main SELECT. Ignore Merged Items as we are in the Surveys tab.
    SELECT DISTINCT
        j.ClientID,
        r.JobID,
        r.JobNo,
        j.SiteID,
        si.Post2000,
        e.FullName [Employee],
        r.RegisterID,
        ISNULL(r.BuildingDesignation, 'No Building Designation') [BuildingDesignation],
        r.SurveyStartDate,
        r.SurveyFinishDate,
        r.SurveyType,
        f.FloorplanID,
        f.FloorNumber,
        f.Description [FloorDescription],
        CASE WHEN f.HasFloorplan = 1 THEN f.FloorplanID ELSE NULL END [FloorplanIDWithData],
        rm.RoomID,
        rm.Number [RoomNumber],
        rm.RoomDisplay [Room],
        s.SampleID,
        s.RegisterItemNo,
        dbo.FormatPortalSampleRef(s.SampleRef) [SampleRef],
        s.SampleRef [SampleRefUnformatted],
        s.AsSample,
        s.SourceDescription,
        s.AsbestosType,
        s.ProductDescription,
        s.MAScore,
        s.PAScore,
        s.RiskScore,
        s.RiskScoreSortOrder,
        s.RiskScoreGroup,
        s.RiskScoreGroupId,
        s.RiskScoreGroupColour,
        s.RecommendedAction,
        s.RecommendedActionColour,
        s.IsMAOnly,
        s.Quantity,
        s.Comments,
        s.SampleResultValue,
        s.Removed,
        CASE
            WHEN s.DateOfNextReviewInt IS NOT NULL THEN DATEADD(MONTH, s.DateOfNextReviewInt, r.SurveyFinishDate)
			WHEN s.DateOfNextReviewIsAsRequired = 1 THEN NULL
            WHEN s.RiskScoreGroupID = 4 THEN DATEADD(MONTH, 6, r.SurveyFinishDate)
            WHEN s.RiskScoreGroupID IS NOT NULL THEN DATEADD(MONTH, 12, r.SurveyFinishDate)
        END [ReinspectionDate],
        s.PhotoID,
        s.DocumentCount,
        CASE WHEN @GetLastNoteCreated = 1
            THEN (SELECT DateCreated FROM @PortalUserNotesData WHERE SampleID = s.SampleID)
            ELSE NULL
        END [LastNoteCreated],
        @TreeviewID [TreeviewID]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientData c ON j.ClientID = c.ClientID
        INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
        INNER JOIN #RegisterData r ON je.JobEmployeeID = r.JobEmployeeID
        LEFT JOIN #FloorplanData f ON f.RegisterID = r.RegisterID
        LEFT JOIN #RoomData rm ON f.FloorplanID = rm.FloorplanID AND ( -- Only show Rooms where a filter matches, or all Rooms that contain a Sample with a filter match.
            rm.ContainsFilteredData = 1
                OR
            EXISTS(SELECT 1 FROM #SampleData WHERE RoomID = rm.RoomID AND ContainsFilteredData = 1)
        )
        LEFT JOIN #SampleData s ON rm.RoomID = s.RoomID AND ( -- Only show Samples where a filter matches, or all Samples in a Room where a Room filter matches.
            s.ContainsFilteredData = 1
                OR
            EXISTS(SELECT 1 FROM #RoomData WHERE RoomID = s.RoomID AND ContainsFilteredData = 1)
        )
    ORDER BY
        s.RecommendedAction,
        s.SampleRef

    -- Clear up temp tables.
    DROP TABLE #RegisterData
    DROP TABLE #FloorplanData
    DROP TABLE #RoomData
    DROP TABLE #SampleData


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetSites]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @PropertyTypeID INT = NULL,
    @Compliance INT, /* 1 = Not Surveyed, 2 = Surveyed, 3 = Post 2000, 4 = UnmanagedSite, 0 or NULL = Any */
    @Surveyed INT, /* 1 = ReinspectionNotRequired, 2 = Compliant, 3 = ReinspectionOverdue, 4 = ReinspectionDueIn3Months, 5 = Post2000, 6 = UnmanagedSite, 0 or NULL = Any */
    @HasSiteDocument INT = NULL, -- 07/11/2016 - Old parameter, remove in the future.
    @GetSiteDocuments INT = 0, /* 0 or NULL = Any, 1 = Yes, 2 = No */
    @ConstructionDate VARCHAR(MAX) = '',
    @StatusFilterTypeID INT = NULL,
    @Other VARCHAR(MAX) = '',
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @Region VARCHAR(MAX) = '',
    @Division VARCHAR(MAX) = '',
    @GetJobCounts BIT = NULL,
    @ForSearch BIT = NULL
/**********************************************************************
** Overview: Get a filtered collection of Sites. This is the main way to get Sites on the Portal.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @PropertyTypeID = NULLIF(@PropertyTypeID, 0),
        @Compliance = NULLIF(@Compliance, 0),
        @Surveyed = NULLIF(@Surveyed, 0),
        @GetSiteDocuments = NULLIF(@GetSiteDocuments, 0),
        @ConstructionDate = NULLIF(LTRIM(RTRIM(@ConstructionDate)), ''),
        @StatusFilterTypeID = NULLIF(@StatusFilterTypeID, 0),
        @Region = NULLIF(LTRIM(RTRIM(@Region)), ''),
        @Division = NULLIF(LTRIM(RTRIM(@Division)), ''),
        @Other = NULLIF(LTRIM(RTRIM(@Other)), ''),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), ''),
        @GetJobCounts = ISNULL(@GetJobCounts, 1),
        @ForSearch = ISNULL(@ForSearch, 0)

    -- Get information that is repeated per row in variables to reduce table scans.
    DECLARE @Sites BIT, @Surveys BIT, @BulkSamples BIT, @AirTests BIT, @Legionella BIT, @b__onlyshowApprovedAirTestsOnPortal BIT, @MoreThanOneSite BIT
    SELECT
        @Sites = pu.Sites,
        @Surveys = pu.Surveys,
        @BulkSamples = pu.BulkSamples,
        @AirTests = pu.AirTests,
        @Legionella = pu.Legionella,
        @b__onlyshowApprovedAirTestsOnPortal = cfg.b__onlyshowApprovedAirTestsOnPortal,
        @MoreThanOneSite = CASE WHEN @SiteIDs IS NULL THEN 1 ELSE CASE WHEN CHARINDEX(',', @SiteIDs) > 0 THEN 1 ELSE 0 END END
    FROM
        PortalUser pu WITH (NOLOCK)
        CROSS JOIN Config cfg WITH (NOLOCK)
    WHERE
        pu.PortalUserID = @PortalUserID

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    CREATE TABLE #SurveyTypeIdData (SurveyTypeID INT PRIMARY KEY)
    IF @SurveyTypeIDs IS NOT NULL
        BEGIN
            INSERT INTO #SurveyTypeIdData (SurveyTypeID)
            SELECT s
            FROM dbo.SplitString(@SurveyTypeIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO #SurveyTypeIdData (SurveyTypeID)
            SELECT SurveyTypeID
            FROM SurveyType WITH (NOLOCK)
            WHERE Deleted IS NULL

            -- Convert #SurveyTypeIdData into a comma separated string of IDs. Reset the variable initially passed in.
            SELECT @SurveyTypeIDs = ''
            SELECT
                @SurveyTypeIDs = STUFF((
                    SELECT ',' + CAST(SurveyTypeID AS VARCHAR(20))
                    FROM #SurveyTypeIdData
                    FOR XML PATH('')), 1, 1, '')
        END

    -- Declare a variable for storing dynamic SQL.
    DECLARE @DynamicSQL NVARCHAR(MAX) = ''

    -- Get all Site Jobs up front to reduce table scans (get the most recent job for each Site).
    -- Only use this if @Surveyed filter is being used or @Compliance = 2.
    CREATE TABLE #SiteJobs (SiteID INT PRIMARY KEY, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SitePost2000 BIT, UnmanagedSite BIT, IsSiteDocument BIT, JobID INT, MaxRegisterFinish DATETIME, HighestRiskScoreGroupID INT, FirstNextReviewDate INT, ReinspectionDate DATETIME, Surveyed INT, DateOfNextReviewIsAsRequired INT, AllNegative INT)

    -- Add an index on important #SiteJobs fields to increase speed below.
    CREATE INDEX temp_SiteJobs ON #SiteJobs (SiteID)

    IF @Surveyed > 0 OR @Compliance = 2
    BEGIN
        -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
        DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

        IF @SiteIDs IS NOT NULL
        BEGIN -- Restriction of Sites.
            INSERT INTO @ClientSiteData (ClientID, SiteID)
            SELECT
                c.ClientID,
                si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
                INNER JOIN (
                    SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
                ) sis ON si.SiteID = sis.SiteID
                LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
                LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
            WHERE
                si.Deleted IS NULL
                    AND
                si.InactiveSite = 0
                    AND
                ( -- Project Filter.
                    (
                        @ProjectGroupID IS NULL
                            AND
                        @ProjectID IS NULL
                    )
                        OR
                    (
                        p.Deleted IS NULL
                            AND
                        (
                            p.ProjectGroupID = @ProjectGroupID
                                OR
                            p.ProjectID = @ProjectID
                        )
                    )
                )
            GROUP BY
                c.ClientID,
                si.SiteID
        END
        ELSE
        BEGIN -- No restriction of Sites.
            INSERT INTO @ClientSiteData (ClientID, SiteID)
            SELECT
                c.ClientID,
                si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
                LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
                LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
            WHERE
                si.Deleted IS NULL
                    AND
                si.InactiveSite = 0
                    AND
                ( -- Project Filter.
                    (
                        @ProjectGroupID IS NULL
                            AND
                        @ProjectID IS NULL
                    )
                        OR
                    (
                        p.Deleted IS NULL
                            AND
                        (
                            p.ProjectGroupID = @ProjectGroupID
                                OR
                            p.ProjectID = @ProjectID
                        )
                    )
                )
            GROUP BY
                c.ClientID,
                si.SiteID
        END

        INSERT INTO #SiteJobs (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish)
        SELECT SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, RegisterFinish [MaxRegisterFinish]
        FROM
        (
            SELECT
                si.SiteID,
                si.Address [SiteAddress],
                si.Postcode [SitePostcode],
                si.Post2000 [SitePost2000],
                si.UnmanagedSite,
                jd.IsSiteDocument,
                jd.JobID,
                jd.RegisterFinish,
                ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.RegisterFinish DESC, jd.JobID DESC) [RowID]
            FROM
                @ClientSiteData csd
                INNER JOIN (
                    SELECT
                        a.*,
                        ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.RegisterFinish DESC, a.JobID DESC) [RowID]
                    FROM
                    (
                        SELECT 0 [IsSiteDocument], j.ClientID, j.SiteID, j.JobID, r.RegisterFinish
                        FROM
                            Job j WITH (NOLOCK)
                            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
                            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
                            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
                            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                            INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                            INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                        WHERE j.Approved IS NOT NULL AND j.Cancelled IS NULL
                        UNION ALL
                        SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], sidi.WorkDate [RegisterFinish]
                        FROM
                            SiteDocument sid WITH (NOLOCK)
                            INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                        WHERE
                            sid.SiteDocumentTypeID = 3 -- Surveys
                                AND
                            sid.Deleted IS NULL
                    ) a
                ) jd ON
                    CASE WHEN jd.IsSiteDocument = 1
                        THEN -1
                        ELSE csd.ClientID
                    END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
                INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
            GROUP BY
                si.SiteID,
                si.Address,
                si.Postcode,
                si.Post2000,
                si.UnmanagedSite,
                jd.IsSiteDocument,
                jd.JobID,
                jd.RegisterFinish,
                jd.RowID
        ) a
        WHERE a.RowID = 1
        GROUP BY
            a.SiteID,
            a.SiteAddress,
            a.SitePostcode,
            a.SitePost2000,
            a.UnmanagedSite,
            a.IsSiteDocument,
            a.JobID,
            a.RegisterFinish,
            a.RowID
        ORDER BY
            a.SiteID

        -- Execute the SPROC PopulateSiteJobsReinspectionState. This populates additional columns in #SiteJobs, so it assumes #SiteJobs already exists.
        EXEC PopulateSiteJobsReinspectionState
    END

    -- Get all Job Data up front to reduce the main SELECT table scans. This temporary table stores one row for each job, but multiple for each site.
    CREATE TABLE #JobData (ClientID INT, SiteID INT, JobID INT, JobNo INT, SurveyTypeID INT, ClientOrderNo VARCHAR(50), Approved DATETIME, RegisterFinish DATETIME, SurveyJobExists BIT, BulkSampleCertificateExists BIT, BulkSampleJobFileName VARCHAR(50), ExternalPhotoID INT)

    -- Add an index on important #JobData fields to increase speed in the main SELECT.
    CREATE INDEX temp_JobData ON #JobData (ClientID, SiteID, JobID, SurveyJobExists, BulkSampleCertificateExists)

    -- Build the dynamic query.
    SELECT @DynamicSQL = @DynamicSQL + 'INSERT INTO #JobData (ClientID, SiteID, JobID, JobNo, SurveyTypeID, ClientOrderNo, Approved, RegisterFinish, SurveyJobExists, BulkSampleCertificateExists, BulkSampleJobFileName, ExternalPhotoID)
    SELECT
        j.ClientID,
        j.SiteID,
        j.JobID,
        j.JobNo,
        ' + CASE WHEN @Surveys = 1 THEN 'MIN(su.SurveyTypeID)' ELSE '0' END + ' [SurveyTypeID],
        j.ClientOrderNo,
        j.Approved,
        ' + CASE WHEN @Surveys = 1 THEN 'MAX(r.RegisterFinish)' ELSE 'NULL' END + ' [RegisterFinish],
        ' + CASE WHEN @Surveys = 1 THEN 'CAST(COUNT(su.SurveyID) AS BIT)' ELSE '0' END + ' [SurveyJobExists],
        ' + CASE WHEN @BulkSamples = 1 THEN 'CASE WHEN bsr.PdfID > 0 THEN 1 ELSE 0 END' ELSE '0' END + ' [BulkSampleCertificateExists],
        ' + CASE WHEN @BulkSamples = 1 THEN 'bsr.FileName' ELSE 'NULL' END + ' [BulkSampleJobFileName],
        ISNULL(MAX(mph.PhotoID), MAX(oph.PhotoID)) [ExternalPhotoID]
    FROM
        Site si WITH (NOLOCK)
        INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
        INNER JOIN Client c WITH (NOLOCK) ON cs.ClientID = c.ClientID AND c.Deleted IS NULL
        ' + CASE WHEN @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL THEN '
        LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID'
            ELSE ''
        END + '
        INNER JOIN Job j WITH (NOLOCK) ON si.SiteID = j.SiteID AND j.Cancelled IS NULL
        ' + CASE WHEN @Surveyed > 0 OR @Compliance = 2 THEN 'INNER JOIN #SiteJobs sj ON si.SiteID = sj.SiteID' ELSE '' END + CASE WHEN @Surveyed > 0 THEN ' AND sj.Surveyed = ' + CAST(@Surveyed AS VARCHAR(20)) ELSE '' END + '
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        LEFT JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID' +
        CASE WHEN @PropertyTypeID IS NOT NULL THEN /* For Property type filter */ '
        LEFT JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
        LEFT JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID'
            ELSE ''
        END + '
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        LEFT JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        ' + CASE WHEN @Surveys = 1 THEN 'LEFT JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID' ELSE '' END + '
        ' + CASE WHEN @BulkSamples = 1 THEN 'OUTER APPLY
        (
            SELECT TOP 1 _pf.PdfID, _pf.FileName
            FROM PDF _pf WITH (NOLOCK)
            WHERE
                _pf.JobID = j.JobID
                    AND
                _pf.DateDeleted IS NULL
                    AND
                _pf.FileName LIKE ''%bsr%''
            ORDER BY
                _pf.DateCreated DESC
        ) bsr' ELSE '' END + '
        ' + CASE WHEN @Legionella = 1 THEN 'LEFT JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL' ELSE '' END + '
        LEFT JOIN Photo mph WITH (NOLOCK) ON r.PhotoID = mph.PhotoID AND mph.ContentType IS NOT NULL AND r.MainExternalPhoto = 1
        LEFT JOIN Photo oph WITH (NOLOCK) ON ISNULL(r.PhotoID, ' + CASE WHEN @Legionella = 1 THEN 'l.PhotoID' ELSE 'NULL' END + ') = oph.PhotoID AND oph.ContentType IS NOT NULL AND ISNULL(r.MainExternalPhoto, 0) = 0
    WHERE
        j.ClientID IN (' + @ClientIDs + ')
            AND
        si.Deleted IS NULL
        ' + CASE WHEN @Surveys = 1 THEN '    AND
        ISNULL(su.SurveyTypeID, -1) IN (-1,' + @SurveyTypeIDs + ')' ELSE '' END + '
            AND
        a.DateDeclined IS NULL
            AND
        (
            r.DateApproved IS NOT NULL
            ' + CASE WHEN @Legionella = 1 THEN 'OR l.DateApproved IS NOT NULL' ELSE '' END + '
        )
    '

    -- Add dynamic WHERE filters
    IF @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectGroupID = ' + CAST(@ProjectGroupID AS VARCHAR(20)) -- project group filter
    END
    IF @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectID = ' + CAST(@ProjectID AS VARCHAR(20))  -- project filter
    END
    IF @SiteIDs IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.SiteID IN (' + @SiteIDs + ')'  -- site filter
    END
    IF @PropertyTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND (ISNULL(asu.PropertyTypeID, al.PropertyTypeID) = ' + CAST(@PropertyTypeID AS VARCHAR(20)) + ')'
    END
    IF @Compliance IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND (' +
            CASE @Compliance
                WHEN 1 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0'
                WHEN 2 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0 AND r.RegisterID IS NOT NULL'
                WHEN 3 THEN 'si.Post2000 = 1'
                WHEN 4 THEN 'si.UnmanagedSite = 1'
            END + ')'
    END
    IF @ConstructionDate IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.ConstructionDate = ''' + CAST(@ConstructionDate AS VARCHAR(20)) + '''' -- construction type filter
    END
    IF @StatusFilterTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.StatusFilterTypeID = ' + CAST(@StatusFilterTypeID AS VARCHAR(20)) -- status filter
    END
    IF @Region IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Region = ''' + @Region + '''' -- site region text filter
    END
    IF @Division IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Division = ''' + @Division + '''' -- site division text filter
    END
    IF @Other IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Other = ''' + @Other + '''' -- site 'other' text filter
    END
    IF @ClientOrderNo IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND j.ClientOrderNo = ''' + @ClientOrderNo + ''''
    END
    IF @UPRN IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.UPRN = ''' + @UPRN + '''' -- site UPRN filter
    END
    IF @AddressSearchString IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ((si.Address LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.PostCode LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.Address + '', '' + ISNULL(si.Postcode, '''') LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.UPRN = ''' + @AddressSearchString + ''')
                OR
            (j.ClientOrderNo = ''' + @AddressSearchString + ''')
        )'
    END
    IF @ForSearch = 1
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.InactiveSite = 0'
    END

    -- Add a GROUP BY
    SELECT @DynamicSQL = @DynamicSQL + '
    GROUP BY
        j.ClientID,
        j.SiteID,
        j.JobID,
        j.JobNo,
        j.ClientOrderNo,
        j.Approved' +
        CASE WHEN @BulkSamples = 1
            THEN ',
        bsr.PdfID,
        bsr.FileName'
            ELSE ''
        END

    -- Execute the SQL to INSERT into temp table
    IF @GetJobCounts = 1
    BEGIN
        EXECUTE sp_executesql @DynamicSQL
    END
    SELECT @DynamicSQL = ''

    -- Build the main dynamic query.
    SELECT @DynamicSQL = @DynamicSQL + 'SELECT
        si.SiteID,
        si.Address,
        si.Postcode,
        ISNULL(si.UPRN, '''') [UPRN],
        si.Contact,
        si.Telephone,
        si.ConstructionDate,
        si.Region,
        si.Division,
        si.Other,
        si.Location.Lat [Latitude],
        si.Location.Long [Longitude],
        si.Post2000,
        si.UnmanagedSite,
        CAST(CASE WHEN sid.SiteDocumentID > 0 THEN 1 ELSE 0 END AS BIT) [DocumentExists],'
        SELECT @DynamicSQL = @DynamicSQL + '
        CASE WHEN MAX(suj.Approved) IS NOT NULL
            THEN ISNULL(
                    CASE WHEN MAX(CAST(c.UseRiskColours AS INT)) = 1
                        THEN MAX(ssros.RiskSampleResultsOverview)
                        ELSE MAX(ssros.RecommendedActionSampleResultsOverview)
                    END, '''')
            ELSE ''''
        END [SampleResultsOverview],
        CASE WHEN MAX(suj.Approved) IS NOT NULL
            THEN
                CASE WHEN MAX(CAST(c.UseRiskColours AS INT)) = 1
                    THEN MAX(ssros.RiskSortOrder)
                    ELSE MAX(ssros.RecommendedActionSortOrder)
                END
            ELSE NULL
        END [SampleResultsOverviewSortOrder],
        CASE WHEN MAX(suj.Approved) IS NOT NULL
            THEN ISNULL(MAX(ssros.RiskSampleResultsOverview), '''')
            ELSE ''''
        END [RiskSampleResultsOverview],
        CASE WHEN MAX(suj.Approved) IS NOT NULL
            THEN ISNULL(MAX(ssros.RecommendedActionSampleResultsOverview), '''')
            ELSE ''''
        END [RecommendedActionSampleResultsOverview],
        ISNULL((SELECT MAX(ExternalPhotoID) FROM #JobData WHERE ClientID = MIN(c.ClientID) AND SiteID = si.SiteID), 0) [ExternalPhotoID],
        sidp.SiteDocumentID [PhotoSiteDocumentID],'
        SELECT @DynamicSQL = @DynamicSQL + '
        MAX(suj.JobID) [SurveyJobID],
        MAX(suj.SurveyTypeID) [SurveyTypeID],
        MAX(jtc.SurveyJobCount) [SurveyJobCount],
        MAX(jtc.BulkSampleJobCount) [BulkSampleJobCount],
        0 [AirTestJobCount], -- 07/11/2016 - Old column, remove in the future.
        0 [LegionellaJobCount], -- 07/11/2016 - Old column, remove in the future.
        ' + CASE WHEN @MoreThanOneSite = 0 THEN '(
            SELECT TOP 1 _pf.FileName
            FROM PDF _pf WITH (NOLOCK)
            WHERE
                _pf.JobID = MAX(suj.JobID)
                    AND
                _pf.DateDeleted IS NULL
                    AND
                _pf.FileName NOT LIKE ''%bsr%''
                    AND
                _pf.FileName NOT LIKE ''%ra%''
                    AND
                _pf.FileName NOT LIKE ''%asb5%''
            ORDER BY
                _pf.DateCreated DESC
        )'
            ELSE 'NULL'
        END + ' [SurveyJobFileName],
        ' + CASE WHEN @MoreThanOneSite = 0 THEN '(
            SELECT TOP 1
                _jd.BulkSampleJobFileName
            FROM
                #JobData _jd
            WHERE
                _jd.ClientID = MIN(c.ClientID)
                    AND
                _jd.SiteID = si.SiteID
                    AND
                _jd.BulkSampleCertificateExists = 1
            ORDER BY
                _jd.RegisterFinish DESC
        )'
            ELSE 'NULL'
        END + ' [BulkSampleJobFileName]'
    SELECT @DynamicSQL = @DynamicSQL + '
    FROM
        Site si WITH (NOLOCK)
        INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
        INNER JOIN Client c WITH (NOLOCK) ON cs.ClientID = c.ClientID
        ' + CASE WHEN @Surveyed > 0 OR @Compliance = 2 THEN 'INNER JOIN #SiteJobs sj ON si.SiteID = sj.SiteID' ELSE '' END + CASE WHEN @Surveyed > 0 THEN ' AND sj.Surveyed = ' + CAST(@Surveyed AS VARCHAR(20)) ELSE '' END + '
        ' + CASE WHEN @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL THEN '
        LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID'
            ELSE ''
        END + '
        OUTER APPLY
        (
            SELECT TOP 1
                _jd.JobID,
                _jd.SurveyTypeID,
                _jd.Approved,
                _jd.ClientOrderNo
            FROM
                #JobData _jd
            WHERE
                _jd.ClientID = c.ClientID
                    AND
                _jd.SiteID = si.SiteID
                    AND
                _jd.SurveyJobExists = 1
            ORDER BY
                _jd.RegisterFinish DESC
        ) suj -- Survey Job'
        SELECT @DynamicSQL = @DynamicSQL + '
        OUTER APPLY
        (
            SELECT
                (SELECT COUNT(*) FROM #JobData WHERE ClientID = c.ClientID AND SiteID = si.SiteID AND SurveyJobExists = 1) [SurveyJobCount],
                (SELECT COUNT(*) FROM #JobData WHERE ClientID = c.ClientID AND SiteID = si.SiteID AND BulkSampleCertificateExists = 1) [BulkSampleJobCount]
        ) jtc -- Job Type Count
        LEFT JOIN SiteSampleResultsOverviewSorting ssros WITH (NOLOCK) ON c.ClientID = ssros.ClientID AND si.SiteID = ssros.SiteID
        OUTER APPLY
        (
            SELECT TOP 1 _sid.SiteDocumentID, _sidt.SiteDocumentTypeID
            FROM
                SiteDocument _sid WITH (NOLOCK)
                LEFT JOIN SiteDocumentType _sidt WITH (NOLOCK) ON _sid.SiteDocumentTypeID = _sidt.SiteDocumentTypeID AND _sidt.Deleted IS NULL AND _sidt.[Default] = 0
            WHERE
                _sid.SiteID = si.SiteID
                    AND
                _sid.Deleted IS NULL
                    AND
                CASE WHEN ' + CAST(@Surveys AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) NOT IN (2, 3) THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@BulkSamples AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 4 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@AirTests AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 5 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@Legionella AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 6 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@Sites AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 7 THEN 1 ELSE 0 END
                END = 1
            ORDER BY _sidt.SiteDocumentTypeID DESC
        ) sid -- Site Documents
        OUTER APPLY
        (
            SELECT TOP 1 _sid.SiteDocumentID
            FROM
                SiteDocument _sid WITH (NOLOCK)
            WHERE
                _sid.SiteID = si.SiteID
                    AND
                _sid.Deleted IS NULL
                    AND
                _sid.SiteDocumentTypeID = 7
            ORDER BY
                _sid.SiteDocumentTypeID DESC
        ) sidp -- Site Document Photo
        ' + CASE WHEN @PropertyTypeID IS NOT NULL /* For Property type filter */ THEN '
        OUTER APPLY
        (
            SELECT TOP 1
                a.AppointmentID,
                ISNULL(asu.PropertyTypeID, al.PropertyTypeID) [PropertyTypeID]
            FROM
                Appointment a WITH (NOLOCK)
                LEFT JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
                LEFT JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
            WHERE
                a.ClientID = c.ClientID
                    AND
                a.SiteID = si.SiteID
                    AND
                a.DateDeclined IS NULL
            ORDER BY
                a.DateCreated DESC
        ) a'
            ELSE ''
        END + '
    WHERE
        c.ClientID IN (' + @ClientIDs + ')
            AND
        si.Deleted IS NULL
    '

    -- Add dynamic WHERE filters
    IF @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.Deleted IS NULL'
    END
    IF @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectGroupID = ' + CAST(@ProjectGroupID AS VARCHAR(20)) -- project group filter
    END
    IF @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectID = ' + CAST(@ProjectID AS VARCHAR(20)) -- project filter
    END
    IF @SiteIDs IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.SiteID IN (' + @SiteIDs + ')' -- site filter
    END
    IF @PropertyTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND a.PropertyTypeID = ' + CAST(@PropertyTypeID AS VARCHAR(20))
    END
    IF @Compliance IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND (' +
            CASE @Compliance
                WHEN 1 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0 AND jtc.SurveyJobCount = 0 AND NOT EXISTS(SELECT 1 FROM SiteDocument WITH (NOLOCK) WHERE SiteID = si.SiteID AND ISNULL(SiteDocumentTypeID, -1) = 3 AND Deleted IS NULL)'
                WHEN 2 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0 AND (jtc.SurveyJobCount > 0 OR sj.IsSiteDocument = 1)'
                WHEN 3 THEN 'si.Post2000 = 1'
                WHEN 4 THEN 'si.UnmanagedSite = 1'
            END + ')'
    END
    IF @GetSiteDocuments IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ' +
            CASE @GetSiteDocuments
                WHEN 1 THEN ' sid.SiteDocumentTypeID IS NOT NULL'
                WHEN 2 THEN ' sid.SiteDocumentTypeID IS NULL'
            END
    END
    IF @ConstructionDate IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.ConstructionDate = ''' + CAST(@ConstructionDate AS VARCHAR(20)) + ''''
    END
    IF @StatusFilterTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.StatusFilterTypeID = ' + CAST(@StatusFilterTypeID AS VARCHAR(20))
    END
    IF @Region IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Region = ''' + @Region + ''''
    END
    IF @Division IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Division = ''' + @Division + ''''
    END
    IF @Other IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Other = ''' + @Other + ''''
    END
    IF @ClientOrderNo IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND suj.ClientOrderNo = ''' + @ClientOrderNo + ''''
    END
    IF @UPRN IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.UPRN = ''' + @UPRN + ''''
    END
    IF @AddressSearchString IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ((si.Address LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.PostCode LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.Address + '', '' + ISNULL(si.Postcode, '''') LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.UPRN = ''' + @AddressSearchString + ''')
                OR
            (suj.ClientOrderNo = ''' + @AddressSearchString + ''')
        )'
    END
    IF @ForSearch = 1
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.InactiveSite = 0'
    END

    -- Add GROUP BY
    SELECT @DynamicSQL = @DynamicSQL + '
    GROUP BY
        si.SiteID,
        si.Address,
        si.Postcode,
        ISNULL(si.UPRN, ''''),
        si.Contact,
        si.Telephone,
        si.ConstructionDate,
        si.Region,
        si.Division,
        si.Other,
        si.Location.Lat,
        si.Location.Long,
        si.Post2000,
        si.UnmanagedSite,
        sid.SiteDocumentID,
        sidp.SiteDocumentID
    '

    -- Add ORDER BY
    SELECT @DynamicSQL = @DynamicSQL + '
    ORDER BY
        SampleResultsOverviewSortOrder DESC,
        ISNULL(si.UPRN, ''''),
        si.Address,
        si.Postcode
    '

    -- Execute the SQL
    EXECUTE sp_executesql @DynamicSQL
    SELECT @DynamicSQL = ''

    -- Clear up temp tables.
    DROP TABLE #ClientIdData
    DROP TABLE #SurveyTypeIdData
    DROP TABLE #SiteJobs
    DROP TABLE #JobData


    SET NOCOUNT OFF;
END



GO
ALTER PROCEDURE [dbo].[GetSites_MainTab]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @PropertyTypeID INT = NULL,
    @Compliance INT, /* 1 = Not Surveyed, 2 = Surveyed, 3 = Post 2000, 4 = UnmanagedSite, 5 = UnmanagedSiteLeg, 6 = SurveyedLeg, 7 = NotSurveyed, 0 or NULL = Any */
    @Surveyed INT, /* 1 = ReinspectionNotRequired, 2 = Compliant, 3 = ReinspectionOverdue, 4 = ReinspectionDueIn3Months, 5 = Post2000, 0 or NULL = Any */
    @HasSiteDocument INT = NULL, -- 07/11/2016 - Old parameter, remove in the future.
    @GetSiteDocuments INT = 0, /* 0 or NULL = Any, 1 = Yes, 2 = No */
    @ConstructionDate VARCHAR(MAX) = '',
    @StatusFilterTypeID INT = NULL,
    @Other VARCHAR(MAX) = '',
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @Region VARCHAR(MAX) = '',
    @Division VARCHAR(MAX) = '',

	-- Paging
	@PerPage INT = 30,
	@CurrentPage INT = 1,
	@OnlyPagingTotals BIT = 0,

	-- Kendo grid order bys
	@OrderByUPRN BIT = 0,
    @OrderByAddress BIT = 0,
    @OrderByPostcode BIT = 0,
    @OrderByContact BIT = 0,
    @OrderByTelephone BIT = 0,
    @OrderBySampleResultsOverviewSortOrder BIT = 0,
    @OrderByAsc BIT = 0,
    @OrderByDesc BIT = 0

/**********************************************************************
** Overview: Get a filtered collection of Sites for the main tab.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;

    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @PropertyTypeID = NULLIF(@PropertyTypeID, 0),
        @Compliance = NULLIF(@Compliance, 0),
        @Surveyed = NULLIF(@Surveyed, 0),
        @GetSiteDocuments = NULLIF(@GetSiteDocuments, 0),
        @ConstructionDate = NULLIF(LTRIM(RTRIM(@ConstructionDate)), ''),
        @StatusFilterTypeID = NULLIF(@StatusFilterTypeID, 0),
        @Region = NULLIF(LTRIM(RTRIM(@Region)), ''),
        @Division = NULLIF(LTRIM(RTRIM(@Division)), ''),
        @Other = NULLIF(LTRIM(RTRIM(@Other)), ''),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), '')

    -- Get information that is repeated per row in variables to reduce table scans.
    DECLARE @Sites BIT, @Surveys BIT, @BulkSamples BIT, @AirTests BIT, @Legionella BIT
    SELECT
        @Sites = pu.Sites,
        @Surveys = pu.Surveys,
        @BulkSamples = pu.BulkSamples,
        @AirTests = pu.AirTests,
        @Legionella = pu.Legionella
    FROM
        PortalUser pu WITH (NOLOCK)
    WHERE
        pu.PortalUserID = @PortalUserID

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    CREATE TABLE #SurveyTypeIdData (SurveyTypeID INT PRIMARY KEY)
    IF @SurveyTypeIDs IS NOT NULL
        BEGIN
            INSERT INTO #SurveyTypeIdData (SurveyTypeID)
            SELECT s
            FROM dbo.SplitString(@SurveyTypeIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO #SurveyTypeIdData (SurveyTypeID)
            SELECT SurveyTypeID
            FROM SurveyType WITH (NOLOCK)
            WHERE Deleted IS NULL

            -- Convert #SurveyTypeIdData into a comma separated string of IDs. Reset the variable initially passed in.
            SELECT @SurveyTypeIDs = ''
            SELECT
                @SurveyTypeIDs = STUFF((
                    SELECT ',' + CAST(SurveyTypeID AS VARCHAR(20))
                    FROM #SurveyTypeIdData
                    FOR XML PATH('')), 1, 1, '')
        END

    -- Declare a variable for storing dynamic SQL.
    DECLARE @DynamicSQL NVARCHAR(MAX) = ''

    -- Get all Site Jobs up front to reduce table scans (get the most recent job for each Site).
    -- Only use this if @Surveyed filter is being used or @Compliance = 2.
    CREATE TABLE #SiteJobs (SiteID INT PRIMARY KEY, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SitePost2000 BIT, UnmanagedSite BIT, IsSiteDocument BIT, JobID INT, MaxRegisterFinish DATETIME, HighestRiskScoreGroupID INT, FirstNextReviewDate INT, ReinspectionDate DATETIME, Surveyed INT, DateOfNextReviewIsAsRequired INT, AllNegative INT) 

    -- Add an index on important #SiteJobs fields to increase speed below.
    CREATE INDEX temp_SiteJobs ON #SiteJobs (SiteID)

        -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
        DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

        IF @SiteIDs IS NOT NULL
        BEGIN -- Restriction of Sites.
            INSERT INTO @ClientSiteData (ClientID, SiteID)
            SELECT
                c.ClientID,
                si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
                INNER JOIN (
                    SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
                ) sis ON si.SiteID = sis.SiteID
                LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
                LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
            WHERE
                si.Deleted IS NULL
                    AND
                si.InactiveSite = 0
                    AND
                ( -- Project Filter.
                    (
                        @ProjectGroupID IS NULL
                            AND
                        @ProjectID IS NULL
                    )
                        OR
                    (
                        p.Deleted IS NULL
                            AND
                        (
                            p.ProjectGroupID = @ProjectGroupID
                                OR
                            p.ProjectID = @ProjectID
                        )
                    )
                )
            GROUP BY
                c.ClientID,
                si.SiteID
        END
        ELSE
        BEGIN -- No restriction of Sites.
            INSERT INTO @ClientSiteData (ClientID, SiteID)
            SELECT
                c.ClientID,
                si.SiteID
            FROM
                #ClientIdData c
                INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
                INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
                LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
                LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
            WHERE
                si.Deleted IS NULL
                    AND
                si.InactiveSite = 0
                    AND
                ( -- Project Filter.
                    (
                        @ProjectGroupID IS NULL
                            AND
                        @ProjectID IS NULL
                    )
                        OR
                    (
                        p.Deleted IS NULL
                            AND
                        (
                            p.ProjectGroupID = @ProjectGroupID
                                OR
                            p.ProjectID = @ProjectID
                        )
                    )
                )
            GROUP BY
                c.ClientID,
                si.SiteID
        END

        INSERT INTO #SiteJobs (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish)
        SELECT SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, RegisterFinish [MaxRegisterFinish]
        FROM
        (
            SELECT
                si.SiteID,
                si.Address [SiteAddress],
                si.Postcode [SitePostcode],
                si.Post2000 [SitePost2000],
                si.UnmanagedSite,
                jd.IsSiteDocument,
                jd.JobID,
                jd.RegisterFinish,
                ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.RegisterFinish DESC, jd.JobID DESC) [RowID]
            FROM
                @ClientSiteData csd
                INNER JOIN (
                    SELECT
                        a.*,
                        ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.RegisterFinish DESC, a.JobID DESC) [RowID]
                    FROM
                    (
                        SELECT 0 [IsSiteDocument], j.ClientID, j.SiteID, j.JobID, r.RegisterFinish
                        FROM
                            Job j WITH (NOLOCK)
                            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
                            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
                            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
                            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                            INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                            INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                        WHERE j.Approved IS NOT NULL AND j.Cancelled IS NULL
                        UNION ALL
                        SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], sidi.WorkDate [RegisterFinish]
                        FROM
                            SiteDocument sid WITH (NOLOCK)
                            INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                        WHERE
                            sid.SiteDocumentTypeID = 3 -- Surveys
                                AND
                            sid.Deleted IS NULL
                    ) a
                ) jd ON
                    CASE WHEN jd.IsSiteDocument = 1
                        THEN -1
                        ELSE csd.ClientID
                    END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
                INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
            GROUP BY
                si.SiteID,
                si.Address,
                si.Postcode,
                si.Post2000,
                si.UnmanagedSite,
                jd.IsSiteDocument,
                jd.JobID,
                jd.RegisterFinish,
                jd.RowID
        ) a
        WHERE a.RowID = 1
        GROUP BY
            a.SiteID,
            a.SiteAddress,
            a.SitePostcode,
            a.SitePost2000,
            a.UnmanagedSite,
            a.IsSiteDocument,
            a.JobID,
            a.RegisterFinish,
            a.RowID
        ORDER BY
            a.SiteID

        -- Execute the SPROC PopulateSiteJobsReinspectionState. This populates additional columns in #SiteJobs, so it assumes #SiteJobs already exists.
	IF @OnlyPagingTotals = 0
	BEGIN
    EXEC PopulateSiteJobsReinspectionState
	END

    -- Build the main dynamic query.
	IF @OnlyPagingTotals = 1
	BEGIN
	SELECT @DynamicSQL = @DynamicSQL + 'SELECT
        1,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        1,
        1,
        1,
        NULL,
        NULL,
        NULL,
        NULL
	'
	END
	IF @OnlyPagingTotals = 0
	BEGIN
    SELECT @DynamicSQL = @DynamicSQL + 'SELECT
        si.SiteID,
        si.Address,
        si.Postcode,
        ISNULL(si.UPRN, '''') [UPRN],
        si.Contact,
        si.Telephone,
        si.ConstructionDate,
        si.Region,
        si.Division,
        si.Other,
        si.Location.Lat [Latitude],
        si.Location.Long [Longitude],
        si.Post2000,
        si.UnmanagedSite,
        CAST(CASE WHEN sid.SiteDocumentID > 0 THEN 1 ELSE 0 END AS BIT) [DocumentExists],
        CASE WHEN MAX(j.Approved) IS NOT NULL
            THEN
                ISNULL(
                    CASE WHEN MAX(CAST(c.UseRiskColours AS INT)) = 1
                        THEN MAX(ssros.RiskSampleResultsOverview)
                        ELSE MAX(ssros.RecommendedActionSampleResultsOverview)
                    END, '''')
            ELSE NULL
        END [SampleResultsOverview],
        CASE WHEN MAX(j.Approved) IS NOT NULL
            THEN
                CASE WHEN MAX(CAST(c.UseRiskColours AS INT)) = 1
                    THEN MAX(ssros.RiskSortOrder)
                    ELSE MAX(ssros.RecommendedActionSortOrder)
                END
            ELSE NULL
        END [SampleResultsOverviewSortOrder],
        CASE WHEN MAX(j.Approved) IS NOT NULL
            THEN ISNULL(MAX(ssros.RiskSampleResultsOverview), '''')
            ELSE ''''
        END [RiskSampleResultsOverview],
        CASE WHEN MAX(j.Approved) IS NOT NULL
            THEN ISNULL(MAX(ssros.RecommendedActionSampleResultsOverview), '''')
            ELSE ''''
        END [RecommendedActionSampleResultsOverview]'
	END
    SELECT @DynamicSQL = @DynamicSQL + '
    FROM
        Site si WITH (NOLOCK)
        INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
        INNER JOIN Client c WITH (NOLOCK) ON cs.ClientID = c.ClientID
        ' + CASE WHEN @Surveyed > 0 OR @Compliance = 2 THEN 'INNER JOIN #SiteJobs sj ON si.SiteID = sj.SiteID' ELSE '' END + CASE WHEN @Surveyed > 0 THEN ' AND sj.Surveyed = ' + CAST(@Surveyed AS VARCHAR(20)) ELSE '' END + '
        ' + CASE WHEN @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL THEN '
        LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID'
            ELSE ''
        END
        SELECT @DynamicSQL = @DynamicSQL + '
        LEFT JOIN SiteSampleResultsOverviewSorting ssros WITH (NOLOCK) ON c.ClientID = ssros.ClientID AND si.SiteID = ssros.SiteID
        OUTER APPLY
        (
            SELECT TOP 1 _sid.SiteDocumentID, _sidt.SiteDocumentTypeID
            FROM
                SiteDocument _sid WITH (NOLOCK)
                LEFT JOIN SiteDocumentType _sidt WITH (NOLOCK) ON _sid.SiteDocumentTypeID = _sidt.SiteDocumentTypeID AND _sidt.Deleted IS NULL AND _sidt.[Default] = 0
            WHERE
                _sid.SiteID = si.SiteID
                    AND
                _sid.Deleted IS NULL
                    AND
                CASE WHEN ' + CAST(@Surveys AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) NOT IN (2, 3) THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@BulkSamples AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 4 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@AirTests AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 5 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@Legionella AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 6 THEN 1 ELSE 0 END
                END = 1
                    AND
                CASE WHEN ' + CAST(@Sites AS VARCHAR(1)) + ' = 1
                    THEN 1
                    ELSE CASE WHEN ISNULL(_sidt.SiteDocumentTypeID, -1) <> 7 THEN 1 ELSE 0 END
                END = 1
            ORDER BY _sidt.SiteDocumentTypeID DESC
        ) sid -- Site Documents
        ' + CASE WHEN @PropertyTypeID IS NOT NULL /* For Property type filter */ THEN '
        OUTER APPLY
        (
            SELECT TOP 1
                a.AppointmentID,
                ISNULL(asu.PropertyTypeID, al.PropertyTypeID) [PropertyTypeID]
            FROM
                Appointment a WITH (NOLOCK)
                LEFT JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
                LEFT JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
            WHERE
                a.ClientID = c.ClientID
                    AND
                a.SiteID = si.SiteID
                    AND
                a.DateDeclined IS NULL
            ORDER BY
                a.DateCreated DESC
        ) a'
            ELSE ''
        END + '
        OUTER APPLY
        (
            SELECT TOP 1
                j.JobID,
                j.Approved,
                j.ClientOrderNo
            FROM
                Job j WITH (NOLOCK)
            WHERE
                j.ClientID = c.ClientID
                    AND
                j.SiteID = si.SiteID
                    AND
                j.Cancelled IS NULL
                    AND
                j.Approved IS NOT NULL
            ORDER BY
                j.Approved DESC
        ) j'

    -- Add a dynamic WHERE filter for the Compliance.
	IF @Compliance IS NOT NULL
	BEGIN
		IF @Compliance IN (6, 7)
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			OUTER APPLY
			(
	            SELECT TOP 1
					j.JobID
				FROM
	                Job j WITH (NOLOCK)
					INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
					INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL					
				WHERE
	                j.ClientID = c.ClientID
						AND
					j.SiteID = si.SiteID
	                    AND
					j.Cancelled IS NULL
	                    AND
					j.Approved IS NOT NULL
				'            
		END
		ELSE
		BEGIN
	        SELECT @DynamicSQL = @DynamicSQL + '
			OUTER APPLY
			(
	            SELECT TOP 1
					j.JobID
				FROM
	                Job j WITH (NOLOCK)
					INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
					INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
					INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
					INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
	                INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
				WHERE
					j.ClientID = c.ClientID
	                    AND
					j.SiteID = si.SiteID
	                    AND
					j.Cancelled IS NULL
	                    AND
					j.Approved IS NOT NULL
				'
	
			-- Add a dynamic WHERE filter for the SurveyTypeIDs.
			IF @SurveyTypeIDs IS NOT NULL
			BEGIN
	            SELECT @DynamicSQL = @DynamicSQL + ' AND su.SurveyTypeID IN (' + @SurveyTypeIDs + ')' + CHAR(13)+CHAR(10)
			END
		END
    -- Build the dynamic query.
    SELECT @DynamicSQL = @DynamicSQL + '
        ) suj -- Survey Job'
    END

    SELECT @DynamicSQL = @DynamicSQL + '
    WHERE
        c.ClientID IN (' + @ClientIDs + ')
            AND
        si.Deleted IS NULL
            AND
        si.InactiveSite = 0
    '

    -- Add dynamic WHERE filters
    IF @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.Deleted IS NULL'
    END
    IF @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectGroupID = ' + CAST(@ProjectGroupID AS VARCHAR(20))
    END
    IF @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectID = ' + CAST(@ProjectID AS VARCHAR(20))
    END
    IF @SiteIDs IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.SiteID IN (' + @SiteIDs + ')'
    END
    IF @PropertyTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND a.PropertyTypeID = ' + CAST(@PropertyTypeID AS VARCHAR(20))
    END
    IF @Compliance IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND (' +
            CASE @Compliance
                WHEN 1 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0 AND suj.JobID IS NULL AND NOT EXISTS(SELECT 1 FROM SiteDocument WITH (NOLOCK) WHERE SiteID = si.SiteID AND ISNULL(SiteDocumentTypeID, -1) = 3 AND Deleted IS NULL)'
                WHEN 2 THEN 'si.Post2000 = 0 AND si.UnmanagedSite = 0 AND (suj.JobID IS NOT NULL OR sj.IsSiteDocument = 1)'
                WHEN 3 THEN 'si.Post2000 = 1'
                WHEN 4 THEN 'si.UnmanagedSite = 1'
				WHEN 5 THEN 'si.UnmanagedSiteLeg = 1'
				WHEN 6 THEN 'si.UnmanagedSiteLeg = 0 AND (suj.JobID IS NOT NULL OR (sid.SiteDocumentID IS NOT NULL AND sid.SiteDocumentTypeID = 6))'
				WHEN 7 THEN 'si.UnmanagedSiteLeg = 0 AND suj.JobID IS NULL AND NOT EXISTS(SELECT 1 FROM SiteDocument WITH (NOLOCK) WHERE SiteID = si.SiteID AND ISNULL(SiteDocumentTypeID, -1) = 6 AND Deleted IS NULL)'
            END + ')'
    END
    IF @GetSiteDocuments IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ' +
            CASE @GetSiteDocuments
                WHEN 1 THEN ' sid.SiteDocumentTypeID IS NOT NULL'
                WHEN 2 THEN ' sid.SiteDocumentTypeID IS NULL'
            END
    END
    IF @ConstructionDate IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.ConstructionDate = ''' + CAST(@ConstructionDate AS VARCHAR(20)) + ''''
    END
    IF @StatusFilterTypeID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.StatusFilterTypeID = ' + CAST(@StatusFilterTypeID AS VARCHAR(20))
    END
    IF @Region IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Region = ''' + @Region + ''''
    END
    IF @Division IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Division = ''' + @Division + ''''
    END
    IF @Other IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.Other = ''' + @Other + ''''
    END
    IF @UPRN IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.UPRN = ''' + @UPRN + ''''
    END
    IF @AddressSearchString IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ((si.Address LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.PostCode LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.Address + '', '' + ISNULL(si.Postcode, '''') LIKE ''%' + @AddressSearchString + '%'')
                OR
            (si.UPRN = ''' + @AddressSearchString + ''')
                OR
            (j.ClientOrderNo = ''' + @AddressSearchString + ''')
        )'
    END

    -- Add GROUP BY
    SELECT @DynamicSQL = @DynamicSQL + '
    GROUP BY
        si.SiteID,
        si.Address,
        si.Postcode,
        ISNULL(si.UPRN, ''''),
        si.Contact,
        si.Telephone,
        si.ConstructionDate,
        si.Region,
        si.Division,
        si.Other,
        si.Location.Lat,
        si.Location.Long,
        si.Post2000,
        si.UnmanagedSite,
        sid.SiteDocumentID
    '

    -- Add ORDER BY
	IF @OnlyPagingTotals = 0
	BEGIN
		IF @OrderByUPRN = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				ISNULL(si.UPRN, '''')' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE IF @OrderByAddress = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				si.Address' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE IF @OrderByPostcode = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				si.Postcode' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE IF @OrderByContact = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				si.Contact' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE IF @OrderByTelephone = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				si.Telephone' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE IF @OrderBySampleResultsOverviewSortOrder = 1
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				SampleResultsOverviewSortOrder' + CASE WHEN @OrderByDesc = 1 THEN ' DESC' ELSE CASE WHEN @OrderByAsc = 1 THEN ' ASC' END END + ',
				si.Address + '','' + si.Postcode
			'
		END
		ELSE
		BEGIN
			SELECT @DynamicSQL = @DynamicSQL + '
			ORDER BY
				si.Address + '', '' + si.Postcode,
				SampleResultsOverviewSortOrder DESC,
				ISNULL(si.UPRN, '''')
			'
		END
	END

	-- Insert the main data into a temp table in order to implement paging
	DECLARE @MainData TABLE (IndexID INT IDENTITY(1, 1), SiteID INT, Address VARCHAR(MAX), Postcode VARCHAR(MAX), UPRN VARCHAR(MAX), Contact VARCHAR(MAX), Telephone VARCHAR(MAX), ConstructionDate VARCHAR(MAX), Region VARCHAR(MAX), Division VARCHAR(MAX), Other VARCHAR(MAX), Latitude FLOAT, Longitude FLOAT, Post2000 BIT, UnmanagedSite BIT, DocumentExists BIT, SampleResultsOverview VARCHAR(MAX), SampleResultsOverviewSortOrder NUMERIC, RiskSampleResultsOverview VARCHAR(MAX), RecommendedActionSampleResultsOverview VARCHAR(MAX))

	INSERT INTO @MainData
    EXECUTE sp_executesql @DynamicSQL
    SELECT @DynamicSQL = ''

	;WITH Paging AS
	(
		SELECT
			CASE
				WHEN @PerPage = 0 
				THEN 1
				ELSE CAST(CEILING(COUNT(*) OVER (PARTITION BY '') * 1.00 / @PerPage) AS INT)
			END [Pages],
			CASE
				WHEN @PerPage = 0
				THEN 1
				ELSE ((ROW_NUMBER() OVER(ORDER BY a.IndexID) - 1) / @PerPage) + 1
			END [Page],
			ROW_NUMBER() OVER(ORDER BY a.IndexID) [RowNumber],
			(SELECT COUNT(*) FROM @MainData) [TotalRowNumber],
			*
		FROM
			(
				SELECT * FROM @MainData
			) a
	)

	-- Main select
	SELECT
		main.Pages,
		main.Page,
		main.RowNumber,
		main.TotalRowNumber,
		main.IndexID,
		main.SiteID,
		main.Address,
		main.Postcode,
		main.UPRN,
		main.Contact,
		main.Telephone,
		main.ConstructionDate,
		main.Region,
		main.Division,
		main.Other,
		main.Latitude,
		main.Longitude,
		main.Post2000,
		main.UnmanagedSite,
		main.DocumentExists,
		main.SampleResultsOverview,
		main.SampleResultsOverviewSortOrder,
		main.RiskSampleResultsOverview,
		main.RecommendedActionSampleResultsOverview

	FROM
		Paging main
	WHERE
		(
		(
			main.RowNumber BETWEEN (@CurrentPage - 1) * @PerPage + 1 AND @CurrentPage * @PerPage
		)
		OR
		(
			@PerPage = 0
				AND
			@CurrentPage = 0
		)
		)
		AND
		(
		(
			@OnlyPagingTotals = 1
			AND
			main.RowNumber = 1
		)
		OR
		(
			@OnlyPagingTotals = 0
		)
		)

    -- Clear up temp tables.
    DROP TABLE #ClientIdData
    DROP TABLE #SurveyTypeIdData
    DROP TABLE #SiteJobs


    SET NOCOUNT OFF;
END

GO


ALTER PROCEDURE [dbo].[GetSitesSurveyed]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @ReturnAsChart BIT = NULL,
    @ForGlance BIT = NULL
/**********************************************************************
** Overview: Get the Sites Surveyed data.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), ''),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0),
        @ForGlance = ISNULL(@ForGlance, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    CREATE TABLE #SurveyTypeIdData (SurveyTypeID INT PRIMARY KEY)
    INSERT INTO #SurveyTypeIdData (SurveyTypeID)
    SELECT s
    FROM dbo.SplitString(@SurveyTypeIDs, ',')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            #ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            #ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get all Site Jobs up front to reduce table scans (get the most recent job for each Site).
    CREATE TABLE #SiteJobs (SiteID INT PRIMARY KEY, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SitePost2000 BIT, UnmanagedSite BIT, IsSiteDocument BIT, JobID INT, MaxRegisterFinish DATETIME, HighestRiskScoreGroupID INT, FirstNextReviewDate INT, ReinspectionDate DATETIME, Surveyed INT, DateOfNextReviewIsAsRequired INT, AllNegative INT)

    -- Add an index on important #SiteJobs fields to increase speed below.
    CREATE INDEX temp_SiteJobs ON #SiteJobs (SiteID)

    INSERT INTO #SiteJobs (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish)
    SELECT SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, RegisterFinish [MaxRegisterFinish]
    FROM
    (
        SELECT
            si.SiteID,
            si.Address [SiteAddress],
            si.Postcode [SitePostcode],
            si.Post2000 [SitePost2000],
            si.UnmanagedSite,
            jd.IsSiteDocument,
            jd.JobID,
            jd.RegisterFinish,
            ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.RegisterFinish DESC, jd.JobID DESC) [RowID]
        FROM
            @ClientSiteData csd
            INNER JOIN (
                SELECT
                    a.*,
                    ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.RegisterFinish DESC, a.JobID DESC) [RowID]
                FROM
                (
                    SELECT 0 [IsSiteDocument], j.ClientID, j.SiteID, j.JobID, r.RegisterFinish
                    FROM
                        Job j WITH (NOLOCK)
                        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
                        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
                        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                        INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                    WHERE j.Approved IS NOT NULL AND j.Cancelled IS NULL
                    UNION ALL
                    SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], sidi.WorkDate [RegisterFinish]
                    FROM
                        SiteDocument sid WITH (NOLOCK)
                        INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                    WHERE
                        sid.SiteDocumentTypeID = 3 -- Surveys
                            AND
                        sid.Deleted IS NULL
                ) a
            ) jd ON
                CASE WHEN jd.IsSiteDocument = 1
                    THEN -1
                    ELSE csd.ClientID
                END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.Post2000,
            si.UnmanagedSite,
            jd.IsSiteDocument,
            jd.JobID,
            jd.RegisterFinish,
            jd.RowID
    ) a
    WHERE a.RowID = 1
    GROUP BY
        a.SiteID,
        a.SiteAddress,
        a.SitePostcode,
        a.SitePost2000,
        a.UnmanagedSite,
        a.IsSiteDocument,
        a.JobID,
        a.RegisterFinish,
        a.RowID
    ORDER BY
        a.SiteID

    -- Execute the SPROC PopulateSiteJobsReinspectionState. This populates additional columns in #SiteJobs, so it assumes #SiteJobs already exists.
    EXEC PopulateSiteJobsReinspectionState

    -- Get the total number of items.
    DECLARE @TotalItems INT = (SELECT COUNT(*) FROM #SiteJobs)

    -- Start the main SELECT.
    IF @ReturnAsChart = 1
    BEGIN
        SELECT
            CASE
                WHEN SitePost2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSite = 1 THEN 'Un-managed Site (No survey required)'
                ELSE
                    CASE Surveyed
                        WHEN 1 THEN 'Reinspection Not Required'
                        WHEN 2 THEN 'Compliant'
                        WHEN 3 THEN 'Reinspection Overdue'
                        WHEN 4 THEN 'Compliant - Reinspection Due Within 3 Months'
                        ELSE NULL
                    END
            END [category],
            CASE
                WHEN SitePost2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSite = 1 THEN '#15527F'
                ELSE
                    CASE Surveyed
                        WHEN 1 THEN '#00B050'
                        WHEN 2 THEN '#AFD8F8'
                        WHEN 3 THEN '#E8412D'
                        WHEN 4 THEN '#F6BD0F'
                        ELSE NULL
                    END
            END [Colour],
            COUNT(*) [Share],
            @TotalItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            Surveyed
        FROM #SiteJobs
        WHERE
            CASE WHEN @ForGlance = 0 -- If for Glance, only return the data that we need.
                THEN 1
                ELSE CASE WHEN Surveyed IN (3,4) THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            CASE
                WHEN SitePost2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSite = 1 THEN 'Un-managed Site (No survey required)'
                ELSE
                    CASE Surveyed
                        WHEN 1 THEN 'Reinspection Not Required'
                        WHEN 2 THEN 'Compliant'
                        WHEN 3 THEN 'Reinspection Overdue'
                        WHEN 4 THEN 'Compliant - Reinspection Due Within 3 Months'
                        ELSE NULL
                    END
            END,
            CASE
                WHEN SitePost2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSite = 1 THEN '#15527F'
                ELSE
                    CASE Surveyed
                        WHEN 1 THEN '#00B050'
                        WHEN 2 THEN '#AFD8F8'
                        WHEN 3 THEN '#E8412D'
                        WHEN 4 THEN '#F6BD0F'
                        ELSE NULL
                    END
            END,
            Surveyed
        ORDER BY
            Surveyed,
            category
    END
    ELSE
    BEGIN
        SELECT
            sj.SiteID,
            sj.JobID,
            0 [SurveyTypeID], -- 07/11/2016 - Old column, remove in the future.
            sj.Surveyed,
            sj.ReinspectionDate,
            sj.SiteAddress [Address],
            sj.SitePostcode [Postcode]
        FROM #SiteJobs sj
        WHERE
            CASE WHEN @ForGlance = 0 -- If for Glance, only return the data that we need.
                THEN 1
                ELSE CASE WHEN sj.Surveyed IN (3,4) THEN 1 ELSE 0 END
            END = 1
    END

    -- Clear up temp tables.
    DROP TABLE #ClientIdData
    DROP TABLE #SurveyTypeIdData
    DROP TABLE #SiteJobs


    SET NOCOUNT OFF;
END


GO
ALTER PROCEDURE [dbo].[GetSamplesByRiskRecAction]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @Risk VARCHAR(100) = '',
    @RecAction VARCHAR(100) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @MaxNumberOfRows INT = 0
/**********************************************************************
** Overview: Get all Samples for a Portal User based on the Risk or Recommended Action.
** If these aren't passed in, it will get all items with a Recommended Action, used on the Sites tab.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @Risk = NULLIF(LTRIM(RTRIM(@Risk)), ''),
        @RecAction = NULLIF(LTRIM(RTRIM(@RecAction)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), '')

    -- Get all Clients up front to reduce table scans on the Clients table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT)
    IF @ClientIDs IS NOT NULL
        BEGIN
            INSERT INTO @ClientIdData (ClientID)
            SELECT s
            FROM dbo.SplitString(@ClientIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO @ClientIdData (ClientID)
            SELECT ClientID
            FROM Client WITH (NOLOCK)
            WHERE Deleted IS NULL
        END

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    DECLARE @SurveyTypeIdData TABLE (SurveyTypeID INT)
    IF @SurveyTypeIDs IS NOT NULL
        BEGIN
            INSERT INTO @SurveyTypeIdData (SurveyTypeID)
            SELECT s
            FROM dbo.SplitString(@SurveyTypeIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO @SurveyTypeIdData (SurveyTypeID)
            SELECT SurveyTypeID
            FROM SurveyType WITH (NOLOCK)
            WHERE Deleted IS NULL
        END

    -- Convert @ClientIdData and @SurveyTypeIdData into a comma separated string of IDs. Reset the variables initially passed in.
    SELECT @ClientIDs = '', @SurveyTypeIDs = ''
    SELECT
        @ClientIDs = STUFF((
            SELECT ',' + CAST(ClientID AS VARCHAR(20))
            FROM @ClientIdData
            FOR XML PATH('')), 1, 1, ''),
        @SurveyTypeIDs = STUFF((
            SELECT ',' + CAST(SurveyTypeID AS VARCHAR(20))
            FROM @SurveyTypeIdData
            FOR XML PATH('')), 1, 1, '')

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @BasementName VARCHAR(50)
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @BasementName = ISNULL(NULLIF(cfg.s__BasementName, ''), 'Z-Sub Level')
    FROM
        Config cfg WITH (NOLOCK)

    -- Declare a variable for storing dynamic SQL.
    DECLARE @DynamicSQL NVARCHAR(MAX) = ''

    -- Build the main dynamic query.
    SELECT @DynamicSQL = @DynamicSQL + 'SELECT DISTINCT' + CASE WHEN @MaxNumberOfRows > 0 THEN ' TOP ' + CAST(@MaxNumberOfRows AS VARCHAR(100)) ELSE '' END + '
        j.JobNo,
        si.Address,
        si.Postcode,
        si.UPRN,
        ISNULL(r.BuildingDesignation, ''No Building Designation'') [Building],
        f.FloorNumber,
        ISNULL(f.DescriptionOverride, REPLACE(dbo.FloorName(f.FloorNumber), ''Z-Sub Level'', ''' + @BasementName + ''')) [FloorDescription],
        rm.RoomCode,
        rm.Number [RoomNumber],
        rm.Description [RoomDescription],
        s.RegisterItemNo,
        s.AsSample,
        s.SampleRef,
        scd.SourceDescription,
        scd.AsbestosType,
        scd.MaterialAssessmentScore,
        scd.PriorityAssessmentScore,
        scd.RiskScore,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        DATENAME(MONTH, scd.TimescaleForCompletion) + '' '' + CAST(DATEPART(YEAR, scd.TimescaleForCompletion) AS VARCHAR(4)) [TimescaleForCompletion],
        su.SurveyTypeID,
        j.JobID,
        s.SampleID,
        scd.IsMAOnly,
        CAST(r.RegisterStart AS DATE) [SurveyStartDate],
        CAST(r.RegisterFinish AS DATE) [SurveyFinishDate]
    FROM
        GuidSamples gs WITH (NOLOCK) 
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON gs.SampleID = scd.SampleID AND gs.ClientID = scd.ClientID AND gs.SiteID = scd.SiteID
        INNER JOIN Sample s WITH (NOLOCK) ON scd.SampleID = s.SampleID
        INNER JOIN Room rm WITH (NOLOCK) ON s.RoomID = rm.RoomID
        INNER JOIN Floorplan f WITH (NOLOCK) ON rm.FloorplanID = f.FloorplanID
        INNER JOIN Register r WITH (NOLOCK) ON f.RegisterID = r.RegisterID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
        INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN Site si WITH (NOLOCK) ON scd.SiteID = si.SiteID
        INNER JOIN Client c WITH (NOLOCK) ON scd.ClientID = c.ClientID
        ' + CASE WHEN @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL THEN '
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID'
                ELSE ''
            END + '
    WHERE
        si.Deleted IS NULL
            AND
        si.InactiveSite = 0
            AND
        si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this table. */
            AND
        c.ClientID IN (' + @ClientIDs + ')
            AND
        su.SurveyTypeID IN (' + @SurveyTypeIDs + ')
            AND
        scd.Removed = 0
            AND
        scd.RecommendedAction IS NOT NULL
    '

    -- Add dynamic WHERE filters
    IF @ProjectGroupID IS NOT NULL OR @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.Deleted IS NULL'
    END
    IF @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectGroupID = ' + CAST(@ProjectGroupID AS VARCHAR(20))
    END
    IF @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectID = ' + CAST(@ProjectID AS VARCHAR(20))
    END
    IF @SiteIDs IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.SiteID IN (' + @SiteIDs + ')'
    END
    IF @Risk = 'Inaccessible'
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND scd.SampleResult <> 0'
    END
    IF @Risk IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ISNULL(scd.RiskScoreGroup, ''Inaccessible'') = ''' + @Risk + ''''
    END
    IF @RecAction IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND scd.RecommendedAction = ''' + @RecAction + ''''
    END

    -- Add ORDER BY
    SELECT @DynamicSQL = @DynamicSQL + '
    ORDER BY
        j.JobNo,
        si.Address,
        si.Postcode,
        si.UPRN,
        Building,
        f.FloorNumber,
        rm.RoomCode,
        rm.Number,
        s.RegisterItemNo
    '

    -- Execute the SQL
    EXECUTE sp_executesql @DynamicSQL
    SELECT @DynamicSQL = ''


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetTargetTimescales]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @ReturnAsChart BIT = NULL
/**********************************************************************
** Overview: Get Target Timescales data.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get all Target Timescales data up front to reduce table scans.
    DECLARE @TimescaleForCompletionData TABLE (SampleID INT NOT NULL, TimescaleForCompletionCategoryID INT NOT NULL)
    INSERT INTO @TimescaleForCompletionData (SampleID, TimescaleForCompletionCategoryID)
    SELECT
        scd.SampleID,
        CASE
            WHEN scd.TimescaleForCompletion < GETDATE() THEN 1                      -- Overdue
            WHEN DATEDIFF(m, GETDATE(), scd.TimescaleForCompletion) <= 3 THEN 2     -- WithinThreeMonths
            WHEN DATEDIFF(m, GETDATE(), scd.TimescaleForCompletion) > 3 THEN 3      -- OverThreeMonths
        END [TimescaleForCompletionCategoryID]
    FROM
        @ClientSiteData csd
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON csd.ClientID = scd.ClientID AND csd.SiteID = scd.SiteID
        INNER JOIN GuidSamples gs WITH (NOLOCK) ON scd.SampleID = gs.SampleID
    WHERE
        scd.TimescaleForCompletion IS NOT NULL

    -- Get the total number of items.
    DECLARE @TotalItems INT = (SELECT COUNT(*) FROM @TimescaleForCompletionData)

    -- Start the main SELECT.
    IF @ReturnAsChart = 1
    BEGIN
        SELECT
            CASE TimescaleForCompletionCategoryID
                WHEN 1 THEN 'Overdue'
                WHEN 2 THEN 'Within 3 Months'
                WHEN 3 THEN 'Over 3 Months'
                ELSE NULL
            END [category],
            CASE TimescaleForCompletionCategoryID
                WHEN 1 THEN '#E8412D'
                WHEN 2 THEN '#FFB400'
                WHEN 3 THEN '#92CE7A'
                ELSE NULL
            END [Colour],
            COUNT(*) [Share],
            @TotalItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend]
        FROM @TimescaleForCompletionData
        GROUP BY
            CASE TimescaleForCompletionCategoryID
                WHEN 1 THEN 'Overdue'
                WHEN 2 THEN 'Within 3 Months'
                WHEN 3 THEN 'Over 3 Months'
                ELSE NULL
            END,
            CASE TimescaleForCompletionCategoryID
                WHEN 1 THEN '#E8412D'
                WHEN 2 THEN '#FFB400'
                WHEN 3 THEN '#92CE7A'
                ELSE NULL
            END
        ORDER BY
            category
    END
    ELSE
    BEGIN
        SELECT * FROM @TimescaleForCompletionData
        ORDER BY SampleID
    END


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetTotalCompliance]
    @PortalUserID INT = NULL,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @SurveyTypeIDs VARCHAR(MAX) = 'NULL',
    @ReturnAsChart BIT = NULL
/**********************************************************************
** Overview: Get Total Compliance data.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), ''),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    DECLARE @SurveyTypeIdData TABLE (SurveyTypeID INT PRIMARY KEY)
    INSERT INTO @SurveyTypeIdData (SurveyTypeID)
    SELECT s
    FROM dbo.SplitString(@SurveyTypeIDs, ',')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
				AND
			si.UnmanagedSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
				AND
			si.UnmanagedSite = 0
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get all Total Compliance data up front to reduce table scans (get the most recent job for each Site).
    CREATE TABLE #TotalComplianceData (SiteID INT PRIMARY KEY, Post2000 BIT NOT NULL, UnmanagedSite BIT NOT NULL, JobID INT, SurveyTypeID INT)

    -- Add an index on important #TotalComplianceData fields to increase speed below.
    CREATE INDEX temp_TotalComplianceData ON #TotalComplianceData (SiteID)

    -- Get Total Compliance data for Sites with a Survey.
    INSERT INTO #TotalComplianceData (SiteID, Post2000, UnmanagedSite, JobID, SurveyTypeID)
    SELECT SiteID, Post2000, UnmanagedSite, JobID, SurveyTypeID
    FROM
    (
        SELECT
            si.SiteID,
            si.Post2000,
            si.UnmanagedSite,
            CASE WHEN si.Post2000 = 0 AND si.UnmanagedSite = 0 THEN ISNULL(jd.JobID, -1) ELSE NULL END [JobID],
            CASE WHEN si.Post2000 = 0 AND si.UnmanagedSite = 0 THEN ISNULL(jd.SurveyTypeID, -1) ELSE NULL END [SurveyTypeID],
            ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.RegisterFinish DESC, jd.JobID DESC) [RowID]
        FROM
            @ClientSiteData csd
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
            INNER JOIN (
                SELECT
                    a.*,
                    ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.RegisterFinish DESC, a.JobID DESC) [RowID]
                FROM
                (
                    SELECT 0 [IsSiteDocument], j.ClientID, j.SiteID, j.JobID, su.SurveyTypeID, r.RegisterFinish
                    FROM
                        Job j WITH (NOLOCK)
                        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
                        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
                        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
                        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
                        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
                        INNER JOIN @SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
                    WHERE j.Approved IS NOT NULL AND j.Cancelled IS NULL
                    UNION ALL
                    SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], NULL [SurveyTypeID], sidi.WorkDate [RegisterFinish]
                    FROM
                        SiteDocument sid WITH (NOLOCK)
                        INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                    WHERE
                        sid.SiteDocumentTypeID = 3 -- Surveys
                            AND
                        sid.Deleted IS NULL
                ) a
            ) jd ON
                CASE WHEN jd.IsSiteDocument = 1
                    THEN -1
                    ELSE csd.ClientID
                END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
        GROUP BY
            si.SiteID,
            si.Post2000,
            si.UnmanagedSite,
            jd.IsSiteDocument,
            jd.JobID,
            jd.SurveyTypeID,
            jd.RegisterFinish,
            jd.RowID
    ) a
    WHERE a.RowID = 1
    GROUP BY
        a.SiteID,
        a.Post2000,
        a.UnmanagedSite,
        a.JobID,
        a.SurveyTypeID
    ORDER BY
        a.SiteID

    -- Get Total Compliance data for Sites without a Survey.
    INSERT INTO #TotalComplianceData (SiteID, Post2000, UnmanagedSite)
    SELECT
        si.SiteID,
        si.Post2000,
        si.UnmanagedSite
    FROM
        @ClientSiteData csd
        INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
    WHERE
        si.SiteID NOT IN (SELECT SiteID FROM #TotalComplianceData)
    GROUP BY
        si.SiteID,
        si.Post2000,
        si.UnmanagedSite

    -- Get the total number of items.
    DECLARE @TotalItems INT = (SELECT COUNT(*) FROM #TotalComplianceData)

    -- Start the main SELECT.
    IF @ReturnAsChart = 1
    BEGIN
        SELECT
            CASE
                WHEN Post2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSite = 1 THEN 'Un-managed Site (No survey required)'
                WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL THEN 'Surveyed'
                ELSE 'Not Surveyed'
            END [category],
            CASE
                WHEN Post2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSite = 1 THEN '#15527F'
                WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL THEN '#AFD8F8'
                ELSE '#F6BD0F'
            END [Colour],
            COUNT(*) [Share],
            @TotalItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            CAST(
                CASE WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL
                    THEN 1
                    ELSE 0
                END AS BIT) [Surveyed]
        FROM #TotalComplianceData
        GROUP BY
            CASE
                WHEN Post2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSite = 1 THEN 'Un-managed Site (No survey required)'
                WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL THEN 'Surveyed'
                ELSE 'Not Surveyed'
            END,
            CASE
                WHEN Post2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSite = 1 THEN '#15527F'
                WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL THEN '#AFD8F8'
                ELSE '#F6BD0F'
            END,
            CASE WHEN JobID IS NOT NULL AND SurveyTypeID IS NOT NULL
                THEN 1
                ELSE 0
            END
        ORDER BY
            Surveyed,
            category
    END
    ELSE
    BEGIN
        SELECT * FROM #TotalComplianceData
        ORDER BY SiteID
    END

    -- Clear up temp tables.
    DROP TABLE #TotalComplianceData


    SET NOCOUNT OFF;
END

GO
ALTER PROCEDURE [dbo].[PopulateSiteJobsReinspectionState]
/**********************************************************************
** Overview: Takes a list of Site/Jobs and sets the reinspection state of each
**   NOTE: Designed to be called within another procedure to populate columns in a temporary
**   table named #SiteJobs. This table must be created with the correct columns before calling this.
**   There must also be a temporary table called #ClientIdData which contains a list of the ClientIDs.
**   As of 20/12/2016, it is being used by these procedures (which are commented appropriately):
**     a) GetSites
**     b) GetSites_MainTab
**     c) GetSitesSurveyed
**     d) GetPortalJobDataForSites
**   This method is used to save duplicating logic between procedures and allowing the complicated logic
**   to calculate the reinspection state to only be run on as few sites/jobs as possible for performance
**   reasons.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- For each Site in #SiteJobs, get all Jobs for that Site into a temp table. We have to do this, as otherwise we could only join based upon the SiteID which doesn't have a filtered list of SurveyTypes.
    CREATE TABLE #AllSiteJobsData (JobID INT PRIMARY KEY NOT NULL, SiteID INT NOT NULL)

    -- Add an index on important #AllSiteJobsData fields to increase speed in the main UPDATE.
    CREATE INDEX temp_AllSiteJobsData ON #AllSiteJobsData (JobID)

    INSERT INTO #AllSiteJobsData (JobID, SiteID)
    SELECT DISTINCT j.JobID, j.SiteID
    FROM
        #SiteJobs sj
        INNER JOIN Job j WITH (NOLOCK) ON sj.SiteID = j.SiteID
        INNER JOIN #ClientIdData c ON j.ClientID = c.ClientID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN #SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
		INNER JOIN SurveyType sutype WITH (NOLOCK) ON sut.SurveyTypeID = sutype.SurveyTypeID
    GROUP BY
        j.JobID, j.SiteID, sutype.Description

    -- Get SampleComputedData data up front to reduce table scans on the SampleComputedData table.
    -- NOTE: Only gets certain fields from SampleComputedData to increase speed. Add more columns in the future if needed.
    -- If more Element data is required in the future, then we will want to move this data into an #ElementData temp table.
    CREATE TABLE #SiteJobsComputedData (SampleComputedDataID INT PRIMARY KEY, SiteID INT, JobID INT, SampleID INT, RiskScoreGroupID INT, DateOfNextReviewInt INT, DateOfNextReviewIsAsRequired BIT NOT NULL, SampleResult INT, IsReinspection BIT, IsManagement BIT)

    -- Add an index on important #SiteJobsComputedData fields to increase speed in the main UPDATE.
    CREATE INDEX temp_SiteJobsComputedData ON #SiteJobsComputedData (SiteID, RiskScoreGroupID, DateOfNextReviewInt, DateOfNextReviewIsAsRequired)

    INSERT INTO #SiteJobsComputedData (SampleComputedDataID, SiteID, JobID, SampleID, RiskScoreGroupID, DateOfNextReviewInt, DateOfNextReviewIsAsRequired, SampleResult, IsReinspection, IsManagement)
    SELECT
        scd.SampleComputedDataID,
        scd.SiteID,
        scd.JobID,
        scd.SampleID,
        scd.RiskScoreGroupID,
        CASE WHEN ISNUMERIC(eim27.ShortDescription) = 1 THEN CAST(eim27.ShortDescription AS INT) END [DateOfNextReviewInt],
		CASE WHEN eim27.ShortDescription = 'As Required' THEN 1 ELSE 0 END [DateOfNextReviewIsAsRequired],
		scd.SampleResult,
		scd.IsReinspection,
		scd.IsManagement
    FROM
        (
            SELECT scd.SampleComputedDataID, scd.SiteID, scd.JobID, scd.SampleID, scd.RiskScoreGroupID, e27.ElementIntMeaningID, scd.SampleResult, CASE WHEN sutype.Description LIKE '%reinspection%' THEN 1 ELSE 0 END [IsReinspection], CASE WHEN sutype.Description LIKE '%management%' THEN 1 ELSE 0 END [IsManagement]
            FROM
                #AllSiteJobsData sj
                INNER JOIN SampleComputedData scd WITH (NOLOCK) ON sj.JobID = scd.JobID AND scd.Removed = 0
                INNER JOIN GuidSamples gs WITH (NOLOCK) ON scd.SampleID = gs.SampleID
                LEFT JOIN Element e27 WITH (NOLOCK) ON scd.SampleId = e27.SampleID AND e27.ElementTypeID = 27
				
				INNER JOIN JobEmployee je WITH (NOLOCK) ON sj.JobID = je.JobID
				INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
				INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
				INNER JOIN SurveyType sutype WITH (NOLOCK) ON su.SurveyTypeID = sutype.SurveyTypeID
            GROUP BY
                scd.SampleComputedDataID, scd.SiteID, scd.JobID, scd.SampleID, scd.RiskScoreGroupID, e27.ElementIntMeaningID, scd.SampleResult, CASE WHEN sutype.Description LIKE '%reinspection%' THEN 1 ELSE 0 END, CASE WHEN sutype.Description LIKE '%management%' THEN 1 ELSE 0 END
        ) scd
        LEFT JOIN ElementIntMeaning eim27 WITH (NOLOCK) ON scd.ElementIntMeaningID = eim27.ElementIntMeaningID

    -- Update temp table #SiteJobs with the data from #SiteJobsComputedData. Does one UPDATE to increase speed (rather than four seperate updates).
    DECLARE @SiteJobsCalculated TABLE (SiteID INT PRIMARY KEY, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SitePost2000 BIT, UnmanagedSite BIT, IsSiteDocument BIT, JobID INT, MaxRegisterFinish DATETIME, HighestRiskScoreGroupID INT, FirstNextReviewDate INT, ReinspectionDate DATETIME, Surveyed INT, DateOfNextReviewIsAsRequired INT, AllNegative INT)
    INSERT INTO @SiteJobsCalculated (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish, HighestRiskScoreGroupID, FirstNextReviewDate, ReinspectionDate, Surveyed, DateOfNextReviewIsAsRequired, AllNegative)
    SELECT
        sj.SiteID,
        sj.SiteAddress,
        sj.SitePostcode,
        sj.SitePost2000,
        sj.UnmanagedSite,
        sj.IsSiteDocument,
        sj.JobID,
        sj.MaxRegisterFinish,
        hrsgi.HighestRiskScoreGroupID,
        fnrd.FirstNextReviewDate,
        rd.ReinspectionDate,
        CASE
            -- A post-2000 Site
            WHEN sj.SitePost2000 = 1 THEN 5
            -- A Un-managed Site
            WHEN sj.UnmanagedSite = 1 THEN 6
            -- ReinspectionNotRequired
            WHEN rd.ReinspectionDate IS NULL AND ISNULL(fnrd.DateOfNextReviewIsAsRequired, 0) = 0 THEN 1
            -- Compliant
            WHEN rd.ReinspectionDate > DATEADD(MONTH, 3, GETDATE()) OR ISNULL(fnrd.DateOfNextReviewIsAsRequired, 0) = 1 THEN 2
            -- ReinspectionOverdue
            WHEN rd.ReinspectionDate < GETDATE() THEN 3
            -- ReinspectionDueIn3Months
            ELSE 4
        END [Surveyed],
		rd.DateOfNextReviewIsAsRequired,
		CASE
			WHEN negCount.Count = 0
			THEN 1
		ELSE
			0
		END [All Negative]
    FROM
        #SiteJobs sj
		OUTER APPLY
		(
			SELECT
				COUNT(*) [Count]
			FROM
				#SiteJobsComputedData sjcd
			WHERE			
				sjcd.SiteID = sj.SiteID
					AND
				sjcd.SampleResult > 0

		) negCount
        OUTER APPLY
        (
            SELECT MAX(RiskScoreGroupID) [HighestRiskScoreGroupID]
            FROM #SiteJobsComputedData
            WHERE SiteID = sj.SiteID
        ) hrsgi -- HighestRiskScoreGroupID
        OUTER APPLY
        (
            SELECT MIN(DateOfNextReviewInt) [FirstNextReviewDate], CAST(MAX(CAST(DateOfNextReviewIsAsRequired AS INT)) AS BIT) [DateOfNextReviewIsAsRequired]
            FROM #SiteJobsComputedData
            WHERE SiteID = sj.SiteID AND RiskScoreGroupID = hrsgi.HighestRiskScoreGroupID AND (DateOfNextReviewInt IS NOT NULL OR DateOfNextReviewIsAsRequired = 1)
        ) fnrd -- FirstNextReviewDate
		OUTER APPLY
		(
			SELECT MAX(sj2.MaxRegisterFinish) [RegFin] FROM #SiteJobs sj2 INNER JOIN #SiteJobsComputedData sjcd ON sj2.JobID = sjcd.JobID WHERE sj2.SiteID = sj.SiteID AND sjcd.IsReinspection = 1
		) ReinspecRegFin
		OUTER APPLY
		(
			SELECT MAX(sj2.MaxRegisterFinish) [RegFin] FROM #SiteJobs sj2 INNER JOIN #SiteJobsComputedData sjcd ON sj2.JobID = sjcd.JobID WHERE sj2.SiteID = sj.SiteID AND sjcd.IsManagement = 1
		) ManageRegFin
        OUTER APPLY
        (
            SELECT
                CASE
                    WHEN sj.IsSiteDocument = 1 THEN DATEADD(MONTH, 12, sj.MaxRegisterFinish)
                    WHEN fnrd.FirstNextReviewDate IS NOT NULL THEN DATEADD(MONTH, fnrd.FirstNextReviewDate, sj.MaxRegisterFinish)
					WHEN fnrd.DateOfNextReviewIsAsRequired = 1 THEN NULL
                    WHEN hrsgi.HighestRiskScoreGroupID = 4 THEN DATEADD(MONTH, 6, sj.MaxRegisterFinish)
                    WHEN hrsgi.HighestRiskScoreGroupID IS NOT NULL THEN DATEADD(MONTH, 12, sj.MaxRegisterFinish)
					--WHEN ReinspecRegFin.RegFin IS NOT NULL THEN DATEADD(MONTH, ISNULL(fnrd.FirstNextReviewDate, 12), ReinspecRegFin.RegFin)
					--WHEN ManageRegFin.RegFin IS NOT NULL THEN DATEADD(MONTH, ISNULL(fnrd.FirstNextReviewDate, 12), ManageRegFin.RegFin)
					ELSE NULL
                END [ReinspectionDate],
				CASE
					WHEN fnrd.DateOfNextReviewIsAsRequired = 1 THEN 1
				END [DateOfNextReviewIsAsRequired]
        ) rd -- ReinspectionDate

    -- Delete from #SiteJobs.
    DELETE FROM #SiteJobs

    -- Re-insert the new data into #SiteJobs.
    INSERT INTO #SiteJobs (SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish, HighestRiskScoreGroupID, FirstNextReviewDate, ReinspectionDate, Surveyed, DateOfNextReviewIsAsRequired, AllNegative)
    SELECT SiteID, SiteAddress, SitePostcode, SitePost2000, UnmanagedSite, IsSiteDocument, JobID, MaxRegisterFinish, HighestRiskScoreGroupID, FirstNextReviewDate, ReinspectionDate, Surveyed, DateOfNextReviewIsAsRequired, AllNegative FROM @SiteJobsCalculated

    -- Clear up temp tables.
    DROP TABLE #AllSiteJobsData
    DROP TABLE #SiteJobsComputedData


    SET NOCOUNT OFF;
END



GO
ALTER PROCEDURE [dbo].[GetSurveys]
    @PortalUserID INT = NULL,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @PropertyTypeID INT = NULL,
    @SurveyTypeID INT = NULL,
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @JobNo INT = 0,
    @JobNoSearch INT = 0,
    @GetSiteDocuments INT = 0, /* 0 or NULL = Any, 1 = Yes, 2 = No */
    @ApprovedFromDate DATETIME = NULL,
    @ApprovedToDate DATETIME = NULL
/**********************************************************************
** Overview: Get a filtered collection of Surveys.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @PropertyTypeID = NULLIF(@PropertyTypeID, 0),
        @SurveyTypeID = NULLIF(@SurveyTypeID, 0),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @JobNo = NULLIF(@JobNo, 0),
        @JobNoSearch = NULLIF(@JobNoSearch, 0),
        @GetSiteDocuments = ISNULL(@GetSiteDocuments, 0)        

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocPropertyTypeID INT = @PropertyTypeID,
        @LocSurveyTypeID INT = @SurveyTypeID,
        @LocClientOrderNo VARCHAR(50) = @ClientOrderNo,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocJobNo INT = @JobNo,
        @LocJobNoSearch INT = @JobNoSearch,
        @LocGetSiteDocuments INT = @GetSiteDocuments,
        @LocApprovedFromDate DATETIME = @ApprovedFromDate,
        @LocApprovedToDate DATETIME = @ApprovedToDate

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END

    -- Get all Surveys Data up front to reduce the main SELECT table scans.
    DECLARE @SurveyData TABLE (IsSiteDocument BIT NOT NULL, JobID INT, JobNo INT, ClientOrderNo VARCHAR(50), Created DATETIME NOT NULL, LastNoteCreated DATETIME, FileName VARCHAR(100), ReportVersions INT NOT NULL, ScopeOfWork VARCHAR(MAX), ProjectID INT, Project VARCHAR(50), ProjectGroup VARCHAR(50), SiteID INT NOT NULL, Address VARCHAR(200) NOT NULL, Postcode VARCHAR(10) NOT NULL, UPRN VARCHAR(50), DateApproved DATETIME, PhotoID INT, SurveyStartDate DATETIME NOT NULL, SurveyFinishDate DATETIME, SurveyTypeID INT, SurveyType VARCHAR(100), SiteDocumentID INT, ImportID INT)

    -- Get normal TEAMS Surveys first.
    IF @LocGetSiteDocuments <> 1
    BEGIN
        INSERT INTO @SurveyData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, FileName, ReportVersions, ScopeOfWork, ProjectID, Project, ProjectGroup, SiteID, Address, Postcode, UPRN, DateApproved, PhotoID, SurveyStartDate, SurveyFinishDate, SurveyTypeID, SurveyType, SiteDocumentID, ImportID)
        SELECT
            0 [IsSiteDocument],
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            MAX(n.DateCreated) [LastNoteCreated],
            pdfFile.FileName [FileName],
            pdfVersions.ReportVersions [ReportVersions],
            CAST(q.ScopeOfWork AS VARCHAR(MAX)) [ScopeOfWork],
            p.ProjectID,
            p.Project,
            p.GroupName [ProjectGroup],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            r.DateApproved,
            NULL [PhotoID], -- 10/11/2016 - Old column, remove in the future.
            CAST(MIN(r.RegisterStart) AS DATE) [SurveyStartDate],
            CAST(MIN(r.RegisterFinish) AS DATE) [SurveyFinishDate],
            sut.SurveyTypeID,
            sut.Description [SurveyType],
            NULL [SiteDocumentID],
			j.ImportID
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN #ClientIdData cid ON j.ClientID = cid.ClientID
            LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
            INNER JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
            LEFT JOIN Note n WITH (NOLOCK) ON j.JobID = n.ItemID AND n.NoteTypeID = 3 AND n.PortalUserID IS NOT NULL
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
            INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
            INNER JOIN SurveyType sut WITH (NOLOCK) ON su.SurveyTypeID = sut.SurveyTypeID
            OUTER APPLY
            (
                SELECT TOP 1 _pf.FileName [FileName]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%bsr%'
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName NOT LIKE '%asb5%'
                ORDER BY
                    _pf.DateCreated DESC
            ) pdfFile
            OUTER APPLY
            (
                SELECT COUNT(_pf.FileName) [ReportVersions]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%bsr%'
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName NOT LIKE '%asb5%'
            ) pdfVersions
        WHERE -- Some of these use a CASE for short circuiting purposes.
            si.Deleted IS NULL
                AND
            j.Cancelled IS NULL
                AND
            je.MainEmployee = 1
                AND
            CASE WHEN @LocApprovedFromDate IS NULL OR @LocApprovedToDate IS NULL -- Approved Date filter. Don't apply when doing a Search.
                THEN 1
                ELSE
                    CASE WHEN j.Approved BETWEEN @LocApprovedFromDate AND @LocApprovedToDate    
                        THEN 1
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocPropertyTypeID IS NULL -- Property Type Filter.
                THEN 1
                ELSE CASE WHEN asu.PropertyTypeID = @LocPropertyTypeID THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocSurveyTypeID IS NULL -- Survey Type Filter.
                THEN 1
                ELSE CASE WHEN sut.SurveyTypeID = @LocSurveyTypeID THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocClientOrderNo IS NULL -- Client Order No Filter.
                THEN 1
                ELSE CASE WHEN j.ClientOrderNo = @LocClientOrderNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                THEN 1
                ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                THEN 1
                ELSE
                    CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR j.ClientOrderNo = @LocAddressSearchString
                        THEN 1
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocJobNo IS NULL -- Job Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocJobNoSearch IS NULL -- Job Search Filter.
                THEN 1
                ELSE CASE WHEN CAST(j.JobNo AS VARCHAR(50)) LIKE CAST(@LocJobNoSearch AS VARCHAR(50)) + '%' THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            pdfFile.FileName,
            pdfVersions.ReportVersions,
            CAST(q.ScopeOfWork AS VARCHAR(MAX)),
            p.ProjectID,
            p.Project,
            p.GroupName,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            r.DateApproved,
            sut.SurveyTypeID,
            sut.Description,
			j.ImportID
        ORDER BY
            r.DateApproved DESC
    END

    -- Get the Site Documents.
    IF @LocGetSiteDocuments <> 2
    BEGIN
        INSERT INTO @SurveyData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, FileName, ReportVersions, ScopeOfWork, ProjectID, Project, ProjectGroup, SiteID, Address, Postcode, UPRN, DateApproved, PhotoID, SurveyStartDate, SurveyFinishDate, SurveyTypeID, SurveyType, SiteDocumentID, ImportID)
        SELECT
            1 [IsSiteDocument],
            NULL [JobID],
            NULL [JobNo],
            NULL [ClientOrderNo],
            sid.Uploaded [Created],
            NULL [LastNoteCreated],
            sid.FileName,
            sidc.SiteDocumentCount [ReportVersions],
            NULL [ScopeOfWork],
            NULL [ProjectID],
            NULL [Project],
            NULL [ProjectGroup],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            NULL [DateApproved],
            NULL [PhotoID],
            sid.WorkDate [SurveyStartDate],
            sid.WorkDate [SurveyFinishDate],
            NULL [SurveyTypeID],
            NULL [SurveyType],
            sid.SiteDocumentID,
			NULL
        FROM
            Site si WITH (NOLOCK)
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
            INNER JOIN #ClientIdData cid ON cs.ClientID = cid.ClientID
            OUTER APPLY
            (
                SELECT TOP 1
                    sid.SiteDocumentID,
                    sid.SiteID,
                    sid.EmployeeID,
                    sid.PortalUserID,
                    sid.FileName,
                    sid.Uploaded,
                    sidi.SiteDocumentInformationID,
                    sidi.WorkDate
                FROM
                    SiteDocument sid WITH (NOLOCK)
                    INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentID = sidi.SiteDocumentID
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 3
                ORDER BY
                    sid.Uploaded DESC
            ) sid
            OUTER APPLY
            (
                SELECT COUNT(*) [SiteDocumentCount]
                FROM SiteDocument sid WITH (NOLOCK)
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 3
            ) sidc
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            sid.SiteDocumentID IS NOT NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            sid.SiteDocumentID,
            sid.FileName,
            sid.Uploaded,
            sid.WorkDate,
            sidc.SiteDocumentCount
        ORDER BY
            sid.Uploaded DESC
    END


    -- Start the main SELECT
    SELECT *
    FROM @SurveyData
    ORDER BY
        ISNULL(DateApproved, Created) DESC,
        IsSiteDocument

    -- Clear up temp tables.
    DROP TABLE #ClientIdData


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaAssetLocationAndOutletData]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = NULL,
    @JobID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL,
    @AllItemsSummary BIT = NULL,
    @IncludeLegionella BIT = NULL,
    @IncludeAssets BIT = NULL,
    @IncludeLocations BIT = NULL,
    @IncludeOutlets BIT = NULL,
    @IncludeQuestions BIT = NULL,
    @IncludeTasks BIT = NULL,
    @LegionellaID INT = NULL,
    @LegionellaAssetID INT = NULL,
    @LegionellaAssetCategoryID INT = NULL,
    @LegionellaLocationID INT = NULL,
    @LegionellaOutletID INT = NULL,
    @LegionellaOutletCategoryID INT = NULL,
    @LegionellaTaskID INT = NULL,
    @LegionellaRiskRatingID INT = NULL,
    @Location VARCHAR(MAX) = NULL,
    @LegionellaSectionID INT = NULL,
    @QuestionsHaveAnswers BIT = 0,
	@QuestionReplaceVariable VARCHAR(50) = NULL
/**********************************************************************
** Overview: Get Legionella Items for the Portal Legionella info page.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- DEBUG
   --     DECLARE
   --         @PortalUserID INT = 58,
   --         @ClientIDs VARCHAR(MAX) = '2475',
   --         @JobID INT = 0,
   --         @SiteIDs VARCHAR(MAX) = '62070',
   --         @AllItemsSummary BIT = 0,
   --         @IncludeLegionella BIT = 1,
   --         @IncludeAssets BIT = 0,
   --         @IncludeLocations BIT = 0,
   --         @IncludeOutlets BIT = 0,
   --         @IncludeQuestions BIT = 1,
   --         @IncludeTasks BIT = 0,
   --         @LegionellaID INT = 0,
   --         @LegionellaAssetID INT = 0,
   --         @LegionellaAssetCategoryID INT = 0,
   --         @LegionellaLocationID INT = 0,
   --         @LegionellaOutletID INT = 0,
   --         @LegionellaOutletCategoryID INT = 0,
   --         @LegionellaTaskID INT = 0,
   --         @LegionellaRiskRatingID INT = 0,
   --         @Location VARCHAR(MAX) = '',
   --         @LegionellaSectionID INT = 0,
   --         @QuestionsHaveAnswers BIT = 1,
			--@QuestionReplaceVariable VARCHAR(50) = '[DateOfNextAssessment]'
    -- END DEBUG

    /* 1. VALIDATE PARAMETERS. */

    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = ISNULL(@ClientIDs, '0'),
        @JobID = ISNULL(@JobID, 0),
        @SiteIDs = ISNULL(@SiteIDs, ''),
        @AllItemsSummary = ISNULL(@AllItemsSummary, 0),
        @IncludeLegionella = ISNULL(@IncludeLegionella, 0),
        @IncludeAssets = ISNULL(@IncludeAssets, 0),
        @IncludeLocations = ISNULL(@IncludeLocations, 0),
        @IncludeOutlets = ISNULL(@IncludeOutlets, 0),
        @IncludeQuestions = ISNULL(@IncludeQuestions, 0),
        @IncludeTasks = ISNULL(@IncludeTasks, 0),
        @LegionellaID = ISNULL(@LegionellaID, 0),
        @LegionellaAssetID = ISNULL(@LegionellaAssetID, 0),
        @LegionellaAssetCategoryID = ISNULL(@LegionellaAssetCategoryID, 0),
        @LegionellaLocationID = ISNULL(@LegionellaLocationID, 0),
        @LegionellaOutletID = ISNULL(@LegionellaOutletID, 0),
        @LegionellaOutletCategoryID = ISNULL(@LegionellaOutletCategoryID, 0),
        @LegionellaTaskID = ISNULL(@LegionellaTaskID, 0),
        @LegionellaRiskRatingID = ISNULL(@LegionellaRiskRatingID, 0),
        @Location = NULLIF(LTRIM(RTRIM(@Location)), ''),
        @LegionellaSectionID = NULLIF(@LegionellaSectionID, 0),
        @QuestionsHaveAnswers = ISNULL(@QuestionsHaveAnswers, 0),
		@QuestionReplaceVariable = NULLIF(LTRIM(RTRIM(@QuestionReplaceVariable)), '')

    -- Check that at least one @Include variable is true.
    IF @IncludeLegionella = 0 AND @IncludeAssets = 0 AND @IncludeLocations = 0 AND @IncludeOutlets = 0 AND @IncludeQuestions = 0 AND @IncludeTasks = 0
    BEGIN
        RAISERROR('No @Include variables set.', 12, 1)
        SET NOEXEC ON;
    END

    -- Set IDs lower down the tree if IDs higher up the tree are set.
    IF (@LegionellaTaskID > 0 AND @LegionellaID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaAssetID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaLocationID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaOutletID = 0)
    BEGIN
        SELECT
            @LegionellaID = ISNULL(LegionellaID, @LegionellaID),
            @LegionellaAssetID = ISNULL(LegionellaAssetID, @LegionellaAssetID),
            @LegionellaLocationID = ISNULL(LegionellaLocationID, @LegionellaLocationID),
            @LegionellaOutletID = ISNULL(LegionellaOutletID, @LegionellaOutletID)
        FROM LegionellaTask WITH (NOLOCK)
        WHERE LegionellaTaskID = @LegionellaTaskID
    END
    IF @LegionellaAssetID > 0 AND @LegionellaID = 0
    BEGIN
        SELECT @LegionellaID = ISNULL(LegionellaID, @LegionellaID)
        FROM LegionellaAsset WITH (NOLOCK)
        WHERE LegionellaAssetID = @LegionellaAssetID
    END
    IF @LegionellaOutletID > 0 AND @LegionellaLocationID = 0
    BEGIN
        SELECT @LegionellaLocationID = ISNULL(LegionellaLocationID, @LegionellaLocationID)
        FROM LegionellaOutlet WITH (NOLOCK)
        WHERE LegionellaOutletID = @LegionellaOutletID
    END
    IF @LegionellaLocationID > 0 AND @LegionellaID = 0
    BEGIN
        SELECT @LegionellaID = ISNULL(LegionellaID, @LegionellaID)
        FROM LegionellaLocation WITH (NOLOCK)
        WHERE LegionellaLocationID = @LegionellaLocationID
    END

    /* 2. GET DATA IN TEMP TABLES. STOP INSERTING IF THE LEVEL IS ABOVE WHAT IS NEEDED. */

    -- Get Legionella data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobNo INT, Approved DATETIME, ClientID INT, SiteID INT, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SiteUPRN VARCHAR(50), JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000))

    INSERT INTO @LegionellaData (JobID, JobNo, Approved, ClientID, SiteID, SiteAddress, SitePostcode, SiteUPRN, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType)
    SELECT DISTINCT
        j.JobID,
        j.JobNo,
        j.Approved,
        j.ClientID,
        si.SiteID,
        si.Address [SiteAddress],
        si.Postcode [SitePostcode],
        si.UPRN [SiteUPRN],
        je.JobEmployeeID,
        e.EmployeeID [SurveyorEmployeeID],
        e.FullName [SurveyorEmployeeName],
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.Notes [LegionellaNotes],
        lp.PhotoID [LegionellaPhotoID],
        lp.PhotoNo [LegionellaPhotoNo],
        l.GUID [LegionellaGUID],
        l.GUIDVersion [LegionellaGUIDVersion],
        lt.LegionellaTypeID,
        lt.Description [LegionellaType]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
        INNER JOIN LegionellaType lt WITH (NOLOCK) ON al.LegionellaTypeID = lt.LegionellaTypeID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        LEFT JOIN Photo lp WITH (NOLOCK) ON l.PhotoID = lp.PhotoID AND lp.ContentType IS NOT NULL
    WHERE
        j.Cancelled IS NULL
            AND
        j.ClientID IN (SELECT s FROM dbo.SplitString(@ClientIDs, ',')) -- For the ClientIDs passed in.
            AND
        (@SiteIDs = '' OR j.SiteID IN (SELECT s FROM dbo.SplitString(@SiteIDs, ','))) -- For the SiteIDs passed in.
            AND
        (j.JobID = @JobID OR @JobID = 0) -- Get for the Job passed in, or for the whole Site.
            AND
        ( -- Get for the LegionellaID passed in or for all the ones on the job / site.
            l.LegionellaID = @LegionellaID
                OR
            @LegionellaID = 0
        )
    ORDER BY
        l.LegionellaStart


    -- Don't insert into @LegionellaAssetData if we don't need Asset data.
    SET NOEXEC OFF;
    IF @IncludeAssets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Asset data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaAssetData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetID INT, AssetPhotoID INT, AssetPhotoNo VARCHAR(50), AssetSystemRef VARCHAR(MAX), AssetLocation VARCHAR(MAX), AssetSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, AssetGUID VARCHAR(50), AssetGUIDVersion INT, LegionellaAssetCategoryID INT, AssetCategory VARCHAR(MAX), AssetCategorySortOrder INT)

    INSERT INTO @LegionellaAssetData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetID, AssetPhotoID, AssetPhotoNo, AssetSystemRef, AssetLocation, AssetSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, AssetGUID, AssetGUIDVersion, LegionellaAssetCategoryID, AssetCategory, AssetCategorySortOrder)
    SELECT DISTINCT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        la.LegionellaAssetID,
        ap.PhotoID [AssetPhotoID],
        ap.PhotoNo [AssetPhotoNo],
        la.SystemRef [AssetSystemRef],
        la.Location [AssetLocation],
        la.SortOrder [AssetSortOrder],
        lrr.LegionellaRiskRatingID [LegionellaAssetRiskRatingID],
        lrr.Description [AssetRiskRating],
        lrr.RiskColour [AssetRiskColour],
        lrr.SortOrder [AssetRiskRatingSortOrder],
        la.GUID [AssetGUID],
        la.GUIDVersion [AssetGUIDVersion],
        lac.LegionellaAssetCategoryID,
        lac.Description [AssetCategory],
        lac.SortOrder [AssetCategorySortOrder]
    FROM
        @LegionellaData l
        LEFT JOIN LegionellaAsset la WITH (NOLOCK) ON l.LegionellaID = la.LegionellaID
        LEFT JOIN LegionellaAssetCategory lac WITH (NOLOCK) ON la.LegionellaAssetCategoryID = lac.LegionellaAssetCategoryID
        LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON la.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
        LEFT JOIN LegionellaAssetOutletDataCollection laodc WITH (NOLOCK) ON la.LegionellaAssetID = laodc.LegionellaAssetID
        LEFT JOIN LegionellaTypeConfig ltc WITH (NOLOCK) ON laodc.LegionellaAssetOutletQuestionID = ltc.LegionellaAssetOutletQuestionID AND l.LegionellaTypeID = ltc.LegionellaTypeID
        LEFT JOIN Photo ap WITH (NOLOCK) ON la.PhotoID = ap.PhotoID AND ap.ContentType IS NOT NULL
    WHERE
        la.Deleted IS NULL
            AND
        ( -- Get for the LegionellaAssetID passed in or for all the ones on the job / site.
            la.LegionellaAssetID = @LegionellaAssetID
                OR
            @LegionellaAssetID = 0
        )
            AND
        ( -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                la.LegionellaAssetID IS NOT NULL
                    AND
                lac.LegionellaAssetCategoryID IS NOT NULL
            )
        )
            AND
        ( -- Get for the Asset Category passed in.
            @LegionellaAssetCategoryID = 0
                OR
            lac.LegionellaAssetCategoryID = @LegionellaAssetCategoryID
        )
            AND -- Location filter.
        (la.Location LIKE '%' + @Location + '%' OR @Location IS NULL)
            AND -- Asset has questions filter.
        CASE WHEN @QuestionsHaveAnswers = 0
            THEN 1
            ELSE
                CASE WHEN @QuestionsHaveAnswers = 1 AND ltc.LegionellaAssetOutletQuestionID IS NOT NULL AND la.Deleted IS NULL
                    THEN 1
                    ELSE 0
                END
        END = 1
    ORDER BY
        l.LegionellaStart,
        la.SortOrder

    -- Don't insert into @LegionellaLocationData if we don't need Location data.
    SET NOEXEC OFF;
    IF @IncludeLocations = 0 AND @IncludeOutlets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Location data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaLocationData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaLocationID INT, Location VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT)

    INSERT INTO @LegionellaLocationData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaLocationID, Location, LocationSortOrder, LocationGUID, LocationGUIDVersion)
    SELECT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        ll.LegionellaLocationID,
        ll.Location,
        ll.SortOrder [LocationSortOrder],
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion]
    FROM
        @LegionellaData l
        LEFT JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID
    WHERE
        ll.Deleted IS NULL -- Not Deleted
            AND
        ( -- Get for the LegionellaLocationID passed in, all the ones for the same GUID, or for all the ones on the job / site.
            ll.LegionellaLocationID = @LegionellaLocationID
                OR
            @LegionellaLocationID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                ll.LegionellaLocationID IS NOT NULL
            )
        )
            AND -- Location filter.
        (ll.Location LIKE '%' + @Location + '%' OR @Location IS NULL)
    ORDER BY
        l.LegionellaStart,
        ll.SortOrder

    -- Don't insert into @LegionellaOutletData if we don't need Outlet data.
    SET NOEXEC OFF;
    IF @IncludeOutlets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Outlet data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaOutletData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaOutletID INT, OutletPhotoID INT, OutletPhotoNo VARCHAR(50), OutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, Location VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletSortOrder INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, OutletGUID VARCHAR(50), OutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaOutletCategoryID INT, OutletCategory VARCHAR(MAX), OutletCategorySortOrder INT)

    INSERT INTO @LegionellaOutletData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaOutletID, OutletPhotoID, OutletPhotoNo, OutletSystemRef, LegionellaLocationID, Location, LocationSortOrder, LocationGUID, LocationGUIDVersion, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletSortOrder, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, OutletGUID, OutletGUIDVersion,  OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaOutletCategoryID, OutletCategory, OutletCategorySortOrder)
    SELECT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        lo.LegionellaOutletID,
        op.PhotoID [OutletPhotoID],
        op.PhotoNo [OutletPhotoNo],
        lo.SystemRef [OutletSystemRef],
        ll.LegionellaLocationID,
        ll.Location,
        ll.LocationSortOrder,
        ll.LocationGUID,
        ll.LegionellaGUIDVersion,
        lo.Cold [OutletCold],
        lo.Hot [OutletHot],
        lo.Mixed [OutletMixed],
        lo.Mains [OutletMains],
        lo.SentinelCold [OutletSentinelCold],
        lo.SentinelHot [OutletSentinelHot],
        lo.SentinelMixed [OutletSentinelMixed],
        lo.SentinelMains [OutletSentinelMains],
        lo.SortOrder [OutletSortOrder],
        lo.FlushedCold [OutletFlushedCold],
        lo.FlushedHot [OutletFlushedHot],
        lo.FlushedMixed [OutletFlushedMixed],
        lo.FlushedMains [OutletFlushedMains],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        sca.OutletSourceColdAssetID,
        sca.OutletSourceColdAssetSystemRef,
        sca.OutletSourceColdAssetLocation,
        sca.OutletSourceColdAssetCategory,
        sha.OutletSourceHotAssetID,
        sha.OutletSourceHotAssetSystemRef,
        sha.OutletSourceHotAssetLocation,
        sha.OutletSourceHotAssetCategory,
        sma.OutletSourceMixedAssetID,
        sma.OutletSourceMixedAssetSystemRef,
        sma.OutletSourceMixedAssetLocation,
        sma.OutletSourceMixedAssetCategory,
        smna.OutletSourceMainsAssetID,
        smna.OutletSourceMainsAssetSystemRef,
        smna.OutletSourceMainsAssetLocation,
        smna.OutletSourceMainsAssetCategory,
        loc.LegionellaOutletCategoryID,
        loc.Description [OutletCategory],
        loc.SortOrder [OutletCategorySortOrder]
    FROM
        @LegionellaData l
        INNER JOIN @LegionellaLocationData ll ON l.LegionellaID = ll.LegionellaID
        LEFT JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID
        LEFT JOIN LegionellaOutletCategory loc WITH (NOLOCK) ON lo.LegionellaOutletCategoryID = loc.LegionellaOutletCategoryID
        LEFT JOIN Photo op WITH (NOLOCK) ON lo.PhotoID = op.PhotoID AND op.ContentType IS NOT NULL
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceColdAssetID],
                _la.SystemRef [OutletSourceColdAssetSystemRef],
                _la.Location [OutletSourceColdAssetLocation],
                _lac.Description [OutletSourceColdAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceCold
        ) sca -- Source Cold Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceHotAssetID],
                _la.SystemRef [OutletSourceHotAssetSystemRef],
                _la.Location [OutletSourceHotAssetLocation],
                _lac.Description [OutletSourceHotAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceHot
        ) sha -- Source Hot Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceMixedAssetID],
                _la.SystemRef [OutletSourceMixedAssetSystemRef],
                _la.Location [OutletSourceMixedAssetLocation],
                _lac.Description [OutletSourceMixedAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceMixed
        ) sma -- Source Mixed Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceMainsAssetID],
                _la.SystemRef [OutletSourceMainsAssetSystemRef],
                _la.Location [OutletSourceMainsAssetLocation],
                _lac.Description [OutletSourceMainsAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceMains
        ) smna -- Source Mains Asset
    WHERE
        lo.Deleted IS NULL
            AND
        ( -- Get for the LegionellaOutletID passed in or for all the ones on the job / site.
            lo.LegionellaOutletID = @LegionellaOutletID
                OR
            @LegionellaOutletID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                lo.LegionellaOutletID IS NOT NULL
            )
        )
            AND
        ( -- Get for the Outlet Category passed in.
            @LegionellaOutletCategoryID = 0
                OR
            loc.LegionellaOutletCategoryID = @LegionellaOutletCategoryID
        )
    ORDER BY
        l.LegionellaStart,
        ll.LocationSortOrder,
        lo.SortOrder

    -- Don't insert into @LegionellaQuestionData if we don't need Question data.
    SET NOEXEC OFF;
    IF @IncludeQuestions = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Question data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaQuestionData TABLE (RowID INT IDENTITY(1,1), RowType INT, JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetOutletID INT, AssetOutletPhotoID INT, AssetOutletPhotoNo VARCHAR(50), AssetOutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, AssetOutletLocation VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, AssetOutletSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, AssetOutletGUID VARCHAR(50), AssetOutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaAssetOutletCategoryID INT, AssetOutletCategory VARCHAR(MAX), AssetOutletCategorySortOrder INT, LegionellaAssetOutletQuestionID INT, QuestionSubTabTitle VARCHAR(MAX), QuestionGroupTitle VARCHAR(MAX), Question VARCHAR(MAX), EntryType VARCHAR(MAX), NullReplace VARCHAR(MAX), ReplaceVariable VARCHAR(MAX), LegionellaAssetOutletQuestionSortOrder INT, LegionellaAssetOutletDataCollectionID INT, DataCollectionText VARCHAR(MAX), DataCollectionInt INT, AnswerText VARCHAR(MAX))

    INSERT INTO @LegionellaQuestionData (RowType, JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetOutletID, AssetOutletPhotoID, AssetOutletPhotoNo, AssetOutletSystemRef, LegionellaLocationID, AssetOutletLocation, LocationSortOrder, LocationGUID, LocationGUIDVersion, AssetOutletSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, AssetOutletGUID, AssetOutletGUIDVersion, OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaAssetOutletCategoryID, AssetOutletCategory, AssetOutletCategorySortOrder, LegionellaAssetOutletQuestionID, QuestionSubTabTitle, QuestionGroupTitle, Question, EntryType, NullReplace, ReplaceVariable, LegionellaAssetOutletQuestionSortOrder, LegionellaAssetOutletDataCollectionID, DataCollectionText, DataCollectionInt, AnswerText)
    SELECT
        lao.RowType,
        lao.JobID,
        lao.JobEmployeeID,
        lao.SurveyorEmployeeID,
        lao.SurveyorEmployeeName,
        lao.LegionellaID,
        lao.BuildingDesignation,
        lao.LegionellaStart,
        lao.LegionellaFinish,
        lao.MonitoringSchedule,
        lao.GeneralDescriptionOfSite,
        lao.ScopeOfWork,
        lao.AreasNotAccessed,
        lao.LegionellaNotes,
        lao.LegionellaPhotoID,
        lao.LegionellaPhotoNo,
        lao.LegionellaGUID,
        lao.LegionellaGUIDVersion,
        lao.LegionellaTypeID,
        lao.LegionellaType,
        lao.LegionellaAssetOutletID,
        lao.AssetOutletPhotoID,
        lao.AssetOutletPhotoNo,
        lao.AssetOutletSystemRef,
        lao.LegionellaLocationID,
        lao.AssetOutletLocation,
        lao.LocationSortOrder,
        lao.LocationGUID,
        lao.LocationGUIDVersion,
        lao.AssetOutletSortOrder,
        lao.LegionellaAssetRiskRatingID,
        lao.AssetRiskRating,
        lao.AssetRiskColour,
        lao.AssetRiskRatingSortOrder,
        lao.OutletCold,
        lao.OutletHot,
        lao.OutletMixed,
        lao.OutletMains,
        lao.OutletSentinelCold,
        lao.OutletSentinelHot,
        lao.OutletSentinelMixed,
        lao.OutletSentinelMains,
        lao.OutletFlushedCold,
        lao.OutletFlushedHot,
        lao.OutletFlushedMixed,
        lao.OutletFlushedMains,
        lao.AssetOutletGUID,
        lao.AssetOutletGUIDVersion,
        lao.OutletSourceColdAssetID,
        lao.OutletSourceColdAssetSystemRef,
        lao.OutletSourceColdAssetLocation,
        lao.OutletSourceColdAssetCategory,
        lao.OutletSourceHotAssetID,
        lao.OutletSourceHotAssetSystemRef,
        lao.OutletSourceHotAssetLocation,
        lao.OutletSourceHotAssetCategory,
        lao.OutletSourceMixedAssetID,
        lao.OutletSourceMixedAssetSystemRef,
        lao.OutletSourceMixedAssetLocation,
        lao.OutletSourceMixedAssetCategory,
        lao.OutletSourceMainsAssetID,
        lao.OutletSourceMainsAssetSystemRef,
        lao.OutletSourceMainsAssetLocation,
        lao.OutletSourceMainsAssetCategory,
        lao.LegionellaAssetOutletCategoryID,
        lao.AssetOutletCategory,
        lao.AssetOutletCategorySortOrder,
        laoq.LegionellaAssetOutletQuestionID,
        laoq.SubTabTitle [QuestionSubTabTitle],
        laoq.GroupTitle [QuestionGroupTitle],
        laoq.Description [Question],
        laoq.EntryType,
        laoq.NullReplace,
        laoq.ReplaceVariable,
        laoq.SortOrder [LegionellaAssetOutletQuestionSortOrder],
        laodc.LegionellaAssetOutletDataCollectionID,
        laodc.DataText [DataCollectionText],
        laodc.DataInt [DataCollectionInt],
        COALESCE(NULLIF(dbo.HtmlEncode(laodc.DataText), ''), CAST(laodc.DataInt AS VARCHAR(MAX)), laoq.NullReplace) [AnswerText]
    FROM
        (
            SELECT
                1 [RowType],
                l.JobID,
                l.JobEmployeeID,
                l.SurveyorEmployeeID,
                l.SurveyorEmployeeName,
                l.LegionellaID,
                l.BuildingDesignation,
                l.LegionellaStart,
                l.LegionellaFinish,
                l.MonitoringSchedule,
                l.GeneralDescriptionOfSite,
                l.ScopeOfWork,
                l.AreasNotAccessed,
                l.LegionellaNotes,
                l.LegionellaPhotoID,
                l.LegionellaPhotoNo,
                l.LegionellaGUID,
                l.LegionellaGUIDVersion,
                l.LegionellaTypeID,
                l.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                NULL [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaData l
            WHERE @IncludeLegionella = 1
                UNION ALL
            SELECT
                2 [RowType],
                la.JobID,
                la.JobEmployeeID,
                la.SurveyorEmployeeID,
                la.SurveyorEmployeeName,
                la.LegionellaID,
                la.BuildingDesignation,
                la.LegionellaStart,
                la.LegionellaFinish,
                la.MonitoringSchedule,
                la.GeneralDescriptionOfSite,
                la.ScopeOfWork,
                la.AreasNotAccessed,
                la.LegionellaNotes,
                la.LegionellaPhotoID,
                la.LegionellaPhotoNo,
                la.LegionellaGUID,
                la.LegionellaGUIDVersion,
                la.LegionellaTypeID,
                la.LegionellaType,
                la.LegionellaAssetID [LegionellaAssetOutletID],
                la.AssetPhotoID [AssetOutletPhotoID],
                la.AssetPhotoNo [AssetOutletPhotoNo],
                la.AssetSystemRef [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                la.AssetLocation [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                la.AssetSortOrder [AssetOutletSortOrder],
                la.LegionellaAssetRiskRatingID,
                la.AssetRiskRating,
                la.AssetRiskColour,
                la.AssetRiskRatingSortOrder,
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                la.AssetGUID [AssetOutletGUID],
                la.AssetGUIDVersion [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                la.LegionellaAssetCategoryID [LegionellaAssetOutletCategoryID],
                la.AssetCategory [AssetOutletCategory],
                la.AssetCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaAssetData la
            WHERE @IncludeAssets = 1
                UNION ALL
            SELECT
                3 [RowType],
                ll.JobID,
                ll.JobEmployeeID,
                ll.SurveyorEmployeeID,
                ll.SurveyorEmployeeName,
                ll.LegionellaID,
                ll.BuildingDesignation,
                ll.LegionellaStart,
                ll.LegionellaFinish,
                ll.MonitoringSchedule,
                ll.GeneralDescriptionOfSite,
                ll.ScopeOfWork,
                ll.AreasNotAccessed,
                ll.LegionellaNotes,
                ll.LegionellaPhotoID,
                ll.LegionellaPhotoNo,
                ll.LegionellaGUID,
                ll.LegionellaGUIDVersion,
                ll.LegionellaTypeID,
                ll.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                ll.LegionellaLocationID,
                ll.Location [AssetOutletLocation],
                ll.LocationSortOrder,
                ll.LocationGUID,
                ll.LocationGUIDVersion,
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaLocationData ll
            WHERE @IncludeLocations = 1
                UNION ALL
            SELECT
                4 [RowType],
                lo.JobID,
                lo.JobEmployeeID,
                lo.SurveyorEmployeeID,
                lo.SurveyorEmployeeName,
                lo.LegionellaID,
                lo.BuildingDesignation,
                lo.LegionellaStart,
                lo.LegionellaFinish,
                lo.MonitoringSchedule,
                lo.GeneralDescriptionOfSite,
                lo.ScopeOfWork,
                lo.AreasNotAccessed,
                lo.LegionellaNotes,
                lo.LegionellaPhotoID,
                lo.LegionellaPhotoNo,
                lo.LegionellaGUID,
                lo.LegionellaGUIDVersion,
                lo.LegionellaTypeID,
                lo.LegionellaType,
                lo.LegionellaOutletID [LegionellaAssetOutletID],
                lo.OutletPhotoID [AssetOutletPhotoID],
                lo.OutletPhotoNo [AssetOutletPhotoNo],
                lo.OutletSystemRef [AssetOutletSystemRef],
                lo.LegionellaLocationID,
                lo.Location [AssetOutletLocation],
                lo.LocationSortOrder,
                lo.LocationGUID,
                lo.LocationGUIDVersion,
                lo.OutletSortOrder [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                lo.OutletCold,
                lo.OutletHot,
                lo.OutletMixed,
                lo.OutletMains,
                lo.OutletSentinelCold,
                lo.OutletSentinelHot,
                lo.OutletSentinelMixed,
                lo.OutletSentinelMains,
                lo.OutletFlushedCold,
                lo.OutletFlushedHot,
                lo.OutletFlushedMixed,
                lo.OutletFlushedMains,
                lo.OutletGUID [AssetOutletGUID],
                lo.OutletGUIDVersion [AssetOutletGUIDVersion],
                lo.OutletSourceColdAssetID,
                lo.OutletSourceColdAssetSystemRef,
                lo.OutletSourceColdAssetLocation,
                lo.OutletSourceColdAssetCategory,
                lo.OutletSourceHotAssetID,
                lo.OutletSourceHotAssetSystemRef,
                lo.OutletSourceHotAssetLocation,
                lo.OutletSourceHotAssetCategory,
                lo.OutletSourceMixedAssetID,
                lo.OutletSourceMixedAssetSystemRef,
                lo.OutletSourceMixedAssetLocation,
                lo.OutletSourceMixedAssetCategory,
                lo.OutletSourceMainsAssetID,
                lo.OutletSourceMainsAssetSystemRef,
                lo.OutletSourceMainsAssetLocation,
                lo.OutletSourceMainsAssetCategory,
                lo.LegionellaOutletCategoryID [LegionellaAssetOutletCategoryID],
                lo.OutletCategory [AssetOutletCategory],
                lo.OutletCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaOutletData lo
            WHERE @IncludeOutlets = 1
        ) lao
        INNER JOIN LegionellaAssetOutletQuestion laoq WITH (NOLOCK) ON
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN 1
                WHEN 3 THEN 1
                ELSE lao.LegionellaAssetOutletCategoryID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN 1 -- Legionella questions handled below
                WHEN 2 THEN laoq.LegionellaAssetCategoryID
                WHEN 3 THEN 1 -- Location questions handled below
                WHEN 4 THEN laoq.LegionellaOutletCategoryID
            END
        INNER JOIN LegionellaTypeConfig ltc WITH (NOLOCK) ON laoq.LegionellaAssetOutletQuestionID = ltc.LegionellaAssetOutletQuestionID AND lao.LegionellaTypeID = ltc.LegionellaTypeID
        LEFT JOIN LegionellaAssetOutletDataCollection laodc WITH (NOLOCK) ON laoq.LegionellaAssetOutletQuestionID = laodc.LegionellaAssetOutletQuestionID AND
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN lao.LegionellaID
                WHEN 3 THEN lao.LegionellaLocationID
                ELSE lao.LegionellaAssetOutletID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN laodc.LegionellaID
                WHEN 2 THEN laodc.LegionellaAssetID
                WHEN 3 THEN laodc.LegionellaLocationID
                WHEN 4 THEN laodc.LegionellaOutletID
            END
    WHERE
        laoq.Deleted IS NULL
            AND
        laodc.Deleted IS NULL
            AND
        (
            lao.RowType IN (2, 4) -- Automatically show Asset and Outlet questions.
                OR
            (
                lao.RowType = 1 -- If something from the Legionella record and a question, decide whether to show or hide the Question.
                    AND
                laoq.LegionellaSectionID IS NOT NULL
            )
                OR
            (
                lao.RowType = 3 -- Filter Locations to only show location questions.
                    AND
                laoq.LegionellaSectionID = 4
            )
        )
            AND
        (@LegionellaSectionID IS NULL OR (@LegionellaSectionID IS NOT NULL AND laoq.LegionellaSectionID = @LegionellaSectionID)) -- For a certain Section or not.
            AND
        (@QuestionsHaveAnswers = 0 OR (@QuestionsHaveAnswers = 1 AND laodc.LegionellaAssetOutletDataCollectionID IS NOT NULL)) -- Pull all questions or only ones with answers.
			AND
		(laoq.ReplaceVariable = @QuestionReplaceVariable OR @QuestionReplaceVariable IS NULL)
    ORDER BY
        lao.LegionellaStart,
        lao.RowType,
        lao.LocationSortOrder,
        lao.AssetOutletSortOrder,
        laoq.SortOrder

    -- Don't insert into @LegionellaTaskData if we don't need Task data.
    SET NOEXEC OFF;
    IF @IncludeTasks = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Task data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaTaskData TABLE (RowID INT IDENTITY(1,1), RowType INT, JobID INT, PDFName VARCHAR(50), JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetOutletID INT, AssetOutletPhotoID INT, AssetOutletPhotoNo VARCHAR(50), AssetOutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, AssetOutletLocation VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, AssetOutletSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, AssetOutletGUID VARCHAR(50), AssetOutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaAssetOutletCategoryID INT, AssetOutletCategory VARCHAR(MAX), AssetOutletCategorySortOrder INT, LegionellaTaskID INT, LegionellaTaskNo INT, TaskRiskDescription VARCHAR(MAX), TaskAction VARCHAR(MAX), TaskSortOrder INT, TaskGUID VARCHAR(50), TaskGUIDVersion INT, LegionellaRiskCategoryID INT, RiskCategory VARCHAR(MAX), RiskCategorySortOrder INT, LegionellaFrequencyCategoryID INT, FrequencyCategory VARCHAR(MAX), FrequencyCategoryDateAddModifier VARCHAR(20), FrequencyCategoryDateAddValue INT, FrequencyCategoryDateCalc DATETIME, FrequencyCategorySortOrder INT, LegionellaRiskRatingID INT, RiskRating VARCHAR(MAX), RiskColour VARCHAR(15), RiskRatingSortOrder INT, LegionellaPriorityRatingID INT, PriorityRating VARCHAR(MAX), ShortPriorityRating VARCHAR(20), PriorityColour VARCHAR(15), PriorityRatingSortOrder INT, LegionellaTaskPhotoID INT)

    INSERT INTO @LegionellaTaskData (RowType, JobID, PDFName, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetOutletID, AssetOutletPhotoID, AssetOutletPhotoNo, AssetOutletSystemRef, LegionellaLocationID, AssetOutletLocation, LocationSortOrder, LocationGUID, LocationGUIDVersion, AssetOutletSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, AssetOutletGUID, AssetOutletGUIDVersion, OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaAssetOutletCategoryID, AssetOutletCategory, AssetOutletCategorySortOrder, LegionellaTaskID, LegionellaTaskNo, TaskRiskDescription, TaskAction, TaskSortOrder, TaskGUID, TaskGUIDVersion, LegionellaRiskCategoryID, RiskCategory, RiskCategorySortOrder, LegionellaFrequencyCategoryID, FrequencyCategory, FrequencyCategoryDateAddModifier, FrequencyCategoryDateAddValue, FrequencyCategoryDateCalc, FrequencyCategorySortOrder, LegionellaRiskRatingID, RiskRating, RiskColour, RiskRatingSortOrder, LegionellaPriorityRatingID, PriorityRating, ShortPriorityRating, PriorityColour, PriorityRatingSortOrder, LegionellaTaskPhotoID)
    SELECT
        lao.RowType,
        lao.JobID,
		pdf.FileName,
        lao.JobEmployeeID,
        lao.SurveyorEmployeeID,
        lao.SurveyorEmployeeName,
        lao.LegionellaID,
        lao.BuildingDesignation,
        lao.LegionellaStart,
        lao.LegionellaFinish,
        lao.MonitoringSchedule,
        lao.GeneralDescriptionOfSite,
        lao.ScopeOfWork,
        lao.AreasNotAccessed,
        lao.LegionellaNotes,
        lao.LegionellaPhotoID,
        lao.LegionellaPhotoNo,
        lao.LegionellaGUID,
        lao.LegionellaGUIDVersion,
        lao.LegionellaTypeID,
        lao.LegionellaType,
        lao.LegionellaAssetOutletID,
        lao.AssetOutletPhotoID,
        lao.AssetOutletPhotoNo,
        lao.AssetOutletSystemRef,
        lao.LegionellaLocationID,
        lao.AssetOutletLocation,
        lao.LocationSortOrder,
        lao.LocationGUID,
        lao.LocationGUIDVersion,
        lao.AssetOutletSortOrder,
        lao.LegionellaAssetRiskRatingID,
        lao.AssetRiskRating,
        lao.AssetRiskColour,
        lao.AssetRiskRatingSortOrder,
        lao.OutletCold,
        lao.OutletHot,
        lao.OutletMixed,
        lao.OutletMains,
        lao.OutletSentinelCold,
        lao.OutletSentinelHot,
        lao.OutletSentinelMixed,
        lao.OutletSentinelMains,
        lao.OutletFlushedCold,
        lao.OutletFlushedHot,
        lao.OutletFlushedMixed,
        lao.OutletFlushedMains,
        lao.AssetOutletGUID,
        lao.AssetOutletGUIDVersion,
        lao.OutletSourceColdAssetID,
        lao.OutletSourceColdAssetSystemRef,
        lao.OutletSourceColdAssetLocation,
        lao.OutletSourceColdAssetCategory,
        lao.OutletSourceHotAssetID,
        lao.OutletSourceHotAssetSystemRef,
        lao.OutletSourceHotAssetLocation,
        lao.OutletSourceHotAssetCategory,
        lao.OutletSourceMixedAssetID,
        lao.OutletSourceMixedAssetSystemRef,
        lao.OutletSourceMixedAssetLocation,
        lao.OutletSourceMixedAssetCategory,
        lao.OutletSourceMainsAssetID,
        lao.OutletSourceMainsAssetSystemRef,
        lao.OutletSourceMainsAssetLocation,
        lao.OutletSourceMainsAssetCategory,
        lao.LegionellaAssetOutletCategoryID,
        lao.AssetOutletCategory,
        lao.AssetOutletCategorySortOrder,
        lt.LegionellaTaskID,
        lt.LegionellaTaskNo,
        lt.RiskDescription [TaskRiskDescription],
        lt.Action [TaskAction],
        lt.SortOrder [TaskSortOrder],
        lt.GUID [TaskGUID],
        lt.GUIDVersion [TaskGUIDVersion],
        lrc.LegionellaRiskCategoryID,
        lrc.Description [RiskCategory],
        lrc.SortOrder [RiskCategorySortOrder],
        lfc.LegionellaFrequencyCategoryID,
        lfc.Description [FrequencyCategory],
        lfc.DateAddModifier [FrequencyCategoryDateAddModifier],
        lfc.DateAddValue [FrequencyCategoryDateAddValue],
        dbo.fn_DateAddFromStringPart(lfc.DateAddModifier, lfc.DateAddValue, lao.MonitoringSchedule) [FrequencyCategoryDateCalc],
        lfc.SortOrder [FrequencyCategorySortOrder],
        lrr.LegionellaRiskRatingID,
        lrr.Description [RiskRating],
        lrr.RiskColour,
        lrr.SortOrder [RiskRatingSortOrder],
        lpr.LegionellaPriorityRatingID,
        lpr.Description [PriorityRating],
        lpr.ShortDescription [ShortPriorityRating],
        lpr.PriorityColour,
        lpr.SortOrder [PriorityRatingSortOrder],
        ltp.PhotoID [LegionellaTaskPhotoID]
    FROM
        (
            SELECT
                1 [RowType],
                l.JobID,
                l.JobEmployeeID,
                l.SurveyorEmployeeID,
                l.SurveyorEmployeeName,
                l.LegionellaID,
                l.BuildingDesignation,
                l.LegionellaStart,
                l.LegionellaFinish,
                l.MonitoringSchedule,
                l.GeneralDescriptionOfSite,
                l.ScopeOfWork,
                l.AreasNotAccessed,
                l.LegionellaNotes,
                l.LegionellaPhotoID,
                l.LegionellaPhotoNo,
                l.LegionellaGUID,
                l.LegionellaGUIDVersion,
                l.LegionellaTypeID,
                l.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                NULL [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaData l
            WHERE @IncludeLegionella = 1
                UNION ALL
            SELECT
                2 [RowType],
                la.JobID,
                la.JobEmployeeID,
                la.SurveyorEmployeeID,
                la.SurveyorEmployeeName,
                la.LegionellaID,
                la.BuildingDesignation,
                la.LegionellaStart,
                la.LegionellaFinish,
                la.MonitoringSchedule,
                la.GeneralDescriptionOfSite,
                la.ScopeOfWork,
                la.AreasNotAccessed,
                la.LegionellaNotes,
                la.LegionellaPhotoID,
                la.LegionellaPhotoNo,
                la.LegionellaGUID,
                la.LegionellaGUIDVersion,
                la.LegionellaTypeID,
                la.LegionellaType,
                la.LegionellaAssetID [LegionellaAssetOutletID],
                la.AssetPhotoID [AssetOutletPhotoID],
                la.AssetPhotoNo [AssetOutletPhotoNo],
                la.AssetSystemRef [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                la.AssetLocation [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                la.AssetSortOrder [AssetOutletSortOrder],
                la.LegionellaAssetRiskRatingID,
                la.AssetRiskRating,
                la.AssetRiskColour,
                la.AssetRiskRatingSortOrder,
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                la.AssetGUID [AssetOutletGUID],
                la.AssetGUIDVersion [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                la.LegionellaAssetCategoryID [LegionellaAssetOutletCategoryID],
                la.AssetCategory [AssetOutletCategory],
                la.AssetCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaAssetData la
            WHERE @IncludeAssets = 1
                UNION ALL
            SELECT
                3 [RowType],
                ll.JobID,
                ll.JobEmployeeID,
                ll.SurveyorEmployeeID,
                ll.SurveyorEmployeeName,
                ll.LegionellaID,
                ll.BuildingDesignation,
                ll.LegionellaStart,
                ll.LegionellaFinish,
                ll.MonitoringSchedule,
                ll.GeneralDescriptionOfSite,
                ll.ScopeOfWork,
                ll.AreasNotAccessed,
                ll.LegionellaNotes,
                ll.LegionellaPhotoID,
                ll.LegionellaPhotoNo,
                ll.LegionellaGUID,
                ll.LegionellaGUIDVersion,
                ll.LegionellaTypeID,
                ll.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                ll.LegionellaLocationID,
                ll.Location [AssetOutletLocation],
                ll.LocationSortOrder,
                ll.LocationGUID,
                ll.LocationGUIDVersion,
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaLocationData ll
            WHERE @IncludeLocations = 1
                UNION ALL
            SELECT
                4 [RowType],
                lo.JobID,
                lo.JobEmployeeID,
                lo.SurveyorEmployeeID,
                lo.SurveyorEmployeeName,
                lo.LegionellaID,
                lo.BuildingDesignation,
                lo.LegionellaStart,
                lo.LegionellaFinish,
                lo.MonitoringSchedule,
                lo.GeneralDescriptionOfSite,
                lo.ScopeOfWork,
                lo.AreasNotAccessed,
                lo.LegionellaNotes,
                lo.LegionellaPhotoID,
                lo.LegionellaPhotoNo,
                lo.LegionellaGUID,
                lo.LegionellaGUIDVersion,
                lo.LegionellaTypeID,
                lo.LegionellaType,
                lo.LegionellaOutletID [LegionellaAssetOutletID],
                lo.OutletPhotoID [AssetOutletPhotoID],
                lo.OutletPhotoNo [AssetOutletPhotoNo],
                lo.OutletSystemRef [AssetOutletSystemRef],
                lo.LegionellaLocationID,
                lo.Location [AssetOutletLocation],
                lo.LocationSortOrder,
                lo.LocationGUID,
                lo.LocationGUIDVersion,
                lo.OutletSortOrder [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                lo.OutletCold,
                lo.OutletHot,
                lo.OutletMixed,
                lo.OutletMains,
                lo.OutletSentinelCold,
                lo.OutletSentinelHot,
                lo.OutletSentinelMixed,
                lo.OutletSentinelMains,
                lo.OutletFlushedCold,
                lo.OutletFlushedHot,
                lo.OutletFlushedMixed,
                lo.OutletFlushedMains,
                lo.OutletGUID [AssetOutletGUID],
                lo.OutletGUIDVersion [AssetOutletGUIDVersion],
                lo.OutletSourceColdAssetID,
                lo.OutletSourceColdAssetSystemRef,
                lo.OutletSourceColdAssetLocation,
                lo.OutletSourceColdAssetCategory,
                lo.OutletSourceHotAssetID,
                lo.OutletSourceHotAssetSystemRef,
                lo.OutletSourceHotAssetLocation,
                lo.OutletSourceHotAssetCategory,
                lo.OutletSourceMixedAssetID,
                lo.OutletSourceMixedAssetSystemRef,
                lo.OutletSourceMixedAssetLocation,
                lo.OutletSourceMixedAssetCategory,
                lo.OutletSourceMainsAssetID,
                lo.OutletSourceMainsAssetSystemRef,
                lo.OutletSourceMainsAssetLocation,
                lo.OutletSourceMainsAssetCategory,
                lo.LegionellaOutletCategoryID [LegionellaAssetOutletCategoryID],
                lo.OutletCategory [AssetOutletCategory],
                lo.OutletCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaOutletData lo
            WHERE @IncludeOutlets = 1
        ) lao
		OUTER APPLY
		(
			SELECT TOP 1
				p.FileName
			FROM
				PDF p
			WHERE
				lao.JobID = p.JobId	
					AND
				p.DateDeleted IS NULL
			ORDER BY
				p.DateCreated DESC
		) pdf
        LEFT JOIN LegionellaTask lt WITH (NOLOCK) ON
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN lao.LegionellaID
                WHEN 3 THEN lao.LegionellaLocationID
                ELSE lao.LegionellaAssetOutletID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN lt.LegionellaID
                WHEN 2 THEN lt.LegionellaAssetID
                WHEN 3 THEN lt.LegionellaLocationID
                WHEN 4 THEN lt.LegionellaOutletID
            END
        LEFT JOIN LegionellaRiskCategory lrc WITH (NOLOCK) ON lt.LegionellaRiskCategoryID = lrc.LegionellaRiskCategoryID
        LEFT JOIN LegionellaFrequencyCategory lfc WITH (NOLOCK) ON lt.LegionellaFrequencyCategoryID = lfc.LegionellaFrequencyCategoryID
        LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
        LEFT JOIN LegionellaPriorityRating lpr WITH (NOLOCK) ON lt.LegionellaPriorityRatingID = lpr.LegionellaPriorityRatingID
        OUTER APPLY
        (
            SELECT TOP 1 PhotoID FROM LegionellaTaskPhoto WITH (NOLOCK) WHERE LegionellaTaskID = lt.LegionellaTaskID
        ) ltp
    WHERE
        lt.Deleted IS NULL
            AND
        ( -- Get for the LegionellaTaskID passed in or for all the ones on the job / site.
            lt.LegionellaTaskID = @LegionellaTaskID
                OR
            @LegionellaTaskID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                lt.LegionellaTaskID IS NOT NULL
            )
        )
            AND
        (lrr.LegionellaRiskRatingID = @LegionellaRiskRatingID OR @LegionellaRiskRatingID = 0)
    ORDER BY
        lao.LegionellaStart,
        lao.RowType,
        lao.LocationSortOrder,
        lao.AssetOutletSortOrder,
        lt.SortOrder

    /* 3. SELECT FROM THE TEMP TABLES. */
    SET NOEXEC OFF;

    IF @IncludeTasks = 1
    BEGIN
        SELECT * FROM @LegionellaTaskData
    END
    ELSE IF @IncludeQuestions = 1
    BEGIN
        SELECT * FROM @LegionellaQuestionData
    END
    ELSE IF @IncludeOutlets = 1
    BEGIN
        SELECT * FROM @LegionellaOutletData
    END
    ELSE IF @IncludeLocations = 1
    BEGIN
        SELECT * FROM @LegionellaLocationData
    END
    ELSE IF @IncludeAssets = 1
    BEGIN
        SELECT * FROM @LegionellaAssetData
    END
    ELSE IF @IncludeLegionella = 1
    BEGIN
        SELECT * FROM @LegionellaData
    END


    SET NOEXEC OFF;
    SET NOCOUNT OFF;
END


GO

ALTER PROCEDURE [dbo].[GetPortalLegionellaAssetLocationAndOutletData_GUID]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = NULL,
    @JobID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL,
    @AllItemsSummary BIT = NULL,
    @IncludeLegionella BIT = NULL,
    @IncludeAssets BIT = NULL,
    @IncludeLocations BIT = NULL,
    @IncludeOutlets BIT = NULL,
    @IncludeQuestions BIT = NULL,
    @IncludeTasks BIT = NULL,
    @LegionellaID INT = NULL,
    @LegionellaAssetID INT = NULL,
    @LegionellaAssetCategoryID INT = NULL,
    @LegionellaLocationID INT = NULL,
    @LegionellaOutletID INT = NULL,
    @LegionellaOutletCategoryID INT = NULL,
    @LegionellaTaskID INT = NULL,
    @LegionellaRiskRatingID INT = NULL,
    @Location VARCHAR(MAX) = NULL,
    @LegionellaSectionID INT = NULL,
    @QuestionsHaveAnswers BIT = 0
/**********************************************************************
** Overview: Get Legionella Items for the Portal Legionella info page.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- DEBUG
        --DECLARE
        --    @PortalUserID INT = 604,
        --    @ClientIDs VARCHAR(MAX) = '8110,8146,149,5351,8139,8145',
        --    @JobID INT = 0,
        --    @SiteIDs VARCHAR(MAX) = '4624',
        --    @AllItemsSummary BIT = 0,
        --    @IncludeLegionella BIT = 0,
        --    @IncludeAssets BIT = 1,
        --    @IncludeLocations BIT = 0,
        --    @IncludeOutlets BIT = 0,
        --    @IncludeQuestions BIT = 0,
        --    @IncludeTasks BIT = 0,
        --    @LegionellaID INT = 0,
        --    @LegionellaAssetID INT = 0,
        --    @LegionellaAssetCategoryID INT = 0,
        --    @LegionellaLocationID INT = 0,
        --    @LegionellaOutletID INT = 0,
        --    @LegionellaOutletCategoryID INT = 0,
        --    @LegionellaTaskID INT = 0,
        --    @LegionellaRiskRatingID INT = 0,
        --    @Location VARCHAR(MAX) = '',
        --    @LegionellaSectionID INT = 0,
        --    @QuestionsHaveAnswers BIT = 1
    -- END DEBUG

    /* 1. VALIDATE PARAMETERS. */

    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = ISNULL(@ClientIDs, '0'),
        @JobID = ISNULL(@JobID, 0),
        @SiteIDs = ISNULL(@SiteIDs, ''),
        @AllItemsSummary = ISNULL(@AllItemsSummary, 0),
        @IncludeLegionella = ISNULL(@IncludeLegionella, 0),
        @IncludeAssets = ISNULL(@IncludeAssets, 0),
        @IncludeLocations = ISNULL(@IncludeLocations, 0),
        @IncludeOutlets = ISNULL(@IncludeOutlets, 0),
        @IncludeQuestions = ISNULL(@IncludeQuestions, 0),
        @IncludeTasks = ISNULL(@IncludeTasks, 0),
        @LegionellaID = ISNULL(@LegionellaID, 0),
        @LegionellaAssetID = ISNULL(@LegionellaAssetID, 0),
        @LegionellaAssetCategoryID = ISNULL(@LegionellaAssetCategoryID, 0),
        @LegionellaLocationID = ISNULL(@LegionellaLocationID, 0),
        @LegionellaOutletID = ISNULL(@LegionellaOutletID, 0),
        @LegionellaOutletCategoryID = ISNULL(@LegionellaOutletCategoryID, 0),
        @LegionellaTaskID = ISNULL(@LegionellaTaskID, 0),
        @LegionellaRiskRatingID = ISNULL(@LegionellaRiskRatingID, 0),
        @Location = NULLIF(LTRIM(RTRIM(@Location)), ''),
        @LegionellaSectionID = NULLIF(@LegionellaSectionID, 0),
        @QuestionsHaveAnswers = ISNULL(@QuestionsHaveAnswers, 0)

    -- Check that at least one @Include variable is true.
    IF @IncludeLegionella = 0 AND @IncludeAssets = 0 AND @IncludeLocations = 0 AND @IncludeOutlets = 0 AND @IncludeQuestions = 0 AND @IncludeTasks = 0
    BEGIN
        RAISERROR('No @Include variables set.', 12, 1)
        SET NOEXEC ON;
    END

    -- Set IDs lower down the tree if IDs higher up the tree are set.
    IF (@LegionellaTaskID > 0 AND @LegionellaID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaAssetID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaLocationID = 0) OR (@LegionellaTaskID > 0 AND @LegionellaOutletID = 0)
    BEGIN
        SELECT
            @LegionellaID = ISNULL(LegionellaID, @LegionellaID),
            @LegionellaAssetID = ISNULL(LegionellaAssetID, @LegionellaAssetID),
            @LegionellaLocationID = ISNULL(LegionellaLocationID, @LegionellaLocationID),
            @LegionellaOutletID = ISNULL(LegionellaOutletID, @LegionellaOutletID)
        FROM LegionellaTask WITH (NOLOCK)
        WHERE LegionellaTaskID = @LegionellaTaskID
    END
    IF @LegionellaAssetID > 0 AND @LegionellaID = 0
    BEGIN
        SELECT @LegionellaID = ISNULL(LegionellaID, @LegionellaID)
        FROM LegionellaAsset WITH (NOLOCK)
        WHERE LegionellaAssetID = @LegionellaAssetID
    END
    IF @LegionellaOutletID > 0 AND @LegionellaLocationID = 0
    BEGIN
        SELECT @LegionellaLocationID = ISNULL(LegionellaLocationID, @LegionellaLocationID)
        FROM LegionellaOutlet WITH (NOLOCK)
        WHERE LegionellaOutletID = @LegionellaOutletID
    END
    IF @LegionellaLocationID > 0 AND @LegionellaID = 0
    BEGIN
        SELECT @LegionellaID = ISNULL(LegionellaID, @LegionellaID)
        FROM LegionellaLocation WITH (NOLOCK)
        WHERE LegionellaLocationID = @LegionellaLocationID
    END

    /* 2. GET DATA IN TEMP TABLES. STOP INSERTING IF THE LEVEL IS ABOVE WHAT IS NEEDED. */

    -- Table for getting all the related GUIDs if the @LegionellaID is passed in as a filter.
    DECLARE @LegionellaIDs TABLE (LegionellaID INT NOT NULL PRIMARY KEY)
    DECLARE @LegionellaGUID VARCHAR(50) = (SELECT GUID FROM Legionella WITH (NOLOCK) WHERE LegionellaID = @LegionellaID)
    IF @LegionellaGUID IS NULL
    BEGIN
        INSERT INTO @LegionellaIDs VALUES (@LegionellaID)
    END
    ELSE
    BEGIN
        INSERT INTO @LegionellaIDs SELECT LegionellaID FROM Legionella WITH (NOLOCK) WHERE GUID = @LegionellaGUID
    END

    -- Get Legionella data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobNo INT, Approved DATETIME, ClientID INT, SiteID INT, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), SiteUPRN VARCHAR(50), JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000))

    INSERT INTO @LegionellaData (JobID, JobNo, Approved, ClientID, SiteID, SiteAddress, SitePostcode, SiteUPRN, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType)
    SELECT DISTINCT
        j.JobID,
        j.JobNo,
        j.Approved,
        j.ClientID,
        si.SiteID,
        si.Address [SiteAddress],
        si.Postcode [SitePostcode],
        si.UPRN [SiteUPRN],
        je.JobEmployeeID,
        e.EmployeeID [SurveyorEmployeeID],
        e.FullName [SurveyorEmployeeName],
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.Notes [LegionellaNotes],
        lp.PhotoID [LegionellaPhotoID],
        lp.PhotoNo [LegionellaPhotoNo],
        l.GUID [LegionellaGUID],
        l.GUIDVersion [LegionellaGUIDVersion],
        lt.LegionellaTypeID,
        lt.Description [LegionellaType]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
        INNER JOIN LegionellaType lt WITH (NOLOCK) ON al.LegionellaTypeID = lt.LegionellaTypeID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        LEFT JOIN Photo lp WITH (NOLOCK) ON l.PhotoID = lp.PhotoID AND lp.ContentType IS NOT NULL
    WHERE
        j.Cancelled IS NULL
            AND
        j.ClientID IN (SELECT s FROM dbo.SplitString(@ClientIDs, ',')) -- For the ClientIDs passed in.
            AND
        (@SiteIDs = '' OR j.SiteID IN (SELECT s FROM dbo.SplitString(@SiteIDs, ','))) -- For the SiteIDs passed in.
            AND
        (j.JobID = @JobID OR @JobID = 0) -- Get for the Job passed in, or for the whole Site.
            AND -- Get for the GUIDs retrieved or for all the ones on the job / site.
        (@LegionellaID = 0 OR l.LegionellaID IN (SELECT LegionellaID FROM @LegionellaIDs))
    ORDER BY
        l.LegionellaStart


    -- Don't insert into @LegionellaAssetData if we don't need Asset data.
    SET NOEXEC OFF;
    IF @IncludeAssets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Table for getting all the related GUIDs if the @LegionellaAssetID is passed in as a filter.
    DECLARE @LegionellaAssetIDs TABLE (LegionellaAssetID INT NOT NULL PRIMARY KEY)
    DECLARE @LegionellaAssetGUID VARCHAR(50) = (SELECT GUID FROM LegionellaAsset WITH (NOLOCK) WHERE LegionellaAssetID = @LegionellaAssetID)
    IF @LegionellaAssetGUID IS NULL
    BEGIN
        INSERT INTO @LegionellaAssetIDs VALUES (@LegionellaAssetID)
    END
    ELSE
    BEGIN
        INSERT INTO @LegionellaAssetIDs SELECT LegionellaAssetID FROM LegionellaAsset WITH (NOLOCK) WHERE GUID = @LegionellaAssetGUID
    END

    -- Get Legionella Asset data up front to reduce the main SELECT table scans. NOTE: The table columns here need to be the same as 'AssetCollection' table below.
    DECLARE @LegionellaAssetData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetID INT, AssetPhotoID INT, AssetPhotoNo VARCHAR(50), AssetSystemRef VARCHAR(MAX), AssetLocation VARCHAR(MAX), AssetSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, AssetGUID VARCHAR(50), AssetGUIDVersion INT, LegionellaAssetCategoryID INT, AssetCategory VARCHAR(MAX), AssetCategorySortOrder INT)

    INSERT INTO @LegionellaAssetData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetID, AssetPhotoID, AssetPhotoNo, AssetSystemRef, AssetLocation, AssetSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, AssetGUID, AssetGUIDVersion, LegionellaAssetCategoryID, AssetCategory, AssetCategorySortOrder)
    SELECT DISTINCT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        la.LegionellaAssetID,
        ap.PhotoID [AssetPhotoID],
        ap.PhotoNo [AssetPhotoNo],
        la.SystemRef [AssetSystemRef],
        la.Location [AssetLocation],
        la.SortOrder [AssetSortOrder],
        lrr.LegionellaRiskRatingID [LegionellaAssetRiskRatingID],
        lrr.Description [AssetRiskRating],
        lrr.RiskColour [AssetRiskColour],
        lrr.SortOrder [AssetRiskRatingSortOrder],
        la.GUID [AssetGUID],
        la.GUIDVersion [AssetGUIDVersion],
        lac.LegionellaAssetCategoryID,
        lac.Description [AssetCategory],
        lac.SortOrder [AssetCategorySortOrder]
    FROM
        @LegionellaData l
        LEFT JOIN LegionellaAsset la WITH (NOLOCK) ON l.LegionellaID = la.LegionellaID
        LEFT JOIN LegionellaAssetCategory lac WITH (NOLOCK) ON la.LegionellaAssetCategoryID = lac.LegionellaAssetCategoryID
        LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON la.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
        LEFT JOIN LegionellaAssetOutletDataCollection laodc WITH (NOLOCK) ON la.LegionellaAssetID = laodc.LegionellaAssetID
        LEFT JOIN LegionellaTypeConfig ltc WITH (NOLOCK) ON laodc.LegionellaAssetOutletQuestionID = ltc.LegionellaAssetOutletQuestionID AND l.LegionellaTypeID = ltc.LegionellaTypeID
        LEFT JOIN Photo ap WITH (NOLOCK) ON la.PhotoID = ap.PhotoID AND ap.ContentType IS NOT NULL
    WHERE
        la.Deleted IS NULL
            AND -- Get for the GUIDs retrieved or for all the ones on the job / site.
        (@LegionellaAssetID = 0 OR la.LegionellaAssetID IN (SELECT LegionellaAssetID FROM @LegionellaAssetIDs))
            AND
        ( -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                la.LegionellaAssetID IS NOT NULL
                    AND
                lac.LegionellaAssetCategoryID IS NOT NULL
            )
        )
            AND
        ( -- Get for the Asset Category passed in.
            @LegionellaAssetCategoryID = 0
                OR
            lac.LegionellaAssetCategoryID = @LegionellaAssetCategoryID
        )
            AND -- Location filter.
        (la.Location LIKE '%' + @Location + '%' OR @Location IS NULL)
            AND -- Asset has questions filter.
        CASE WHEN @QuestionsHaveAnswers = 0
            THEN 1
            ELSE
                CASE WHEN @QuestionsHaveAnswers = 1 AND ltc.LegionellaAssetOutletQuestionID IS NOT NULL AND la.Deleted IS NULL
                    THEN 1
                    ELSE 0
                END
        END = 1
    ORDER BY
        l.LegionellaStart,
        la.SortOrder

    -- Don't insert into @LegionellaLocationData if we don't need Location data.
    SET NOEXEC OFF;
    IF @IncludeLocations = 0 AND @IncludeOutlets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Location data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaLocationData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaLocationID INT, Location VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT)

    INSERT INTO @LegionellaLocationData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaLocationID, Location, LocationSortOrder, LocationGUID, LocationGUIDVersion)
    SELECT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        ll.LegionellaLocationID,
        ll.Location,
        ll.SortOrder [LocationSortOrder],
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion]
    FROM
        @LegionellaData l
        LEFT JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID
    WHERE
        ll.Deleted IS NULL -- Not Deleted
            AND
        ( -- Get for the LegionellaLocationID passed in, all the ones for the same GUID, or for all the ones on the job / site.
            ll.LegionellaLocationID = @LegionellaLocationID
                OR
            @LegionellaLocationID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                ll.LegionellaLocationID IS NOT NULL
            )
        )
            AND -- Location filter.
        (ll.Location LIKE '%' + @Location + '%' OR @Location IS NULL)
    ORDER BY
        l.LegionellaStart,
        ll.SortOrder

    -- Don't insert into @LegionellaOutletData if we don't need Outlet data.
    SET NOEXEC OFF;
    IF @IncludeOutlets = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Outlet data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaOutletData TABLE (RowID INT IDENTITY(1,1), JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaOutletID INT, OutletPhotoID INT, OutletPhotoNo VARCHAR(50), OutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, Location VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletSortOrder INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, OutletGUID VARCHAR(50), OutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaOutletCategoryID INT, OutletCategory VARCHAR(MAX), OutletCategorySortOrder INT)

    INSERT INTO @LegionellaOutletData (JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaOutletID, OutletPhotoID, OutletPhotoNo, OutletSystemRef, LegionellaLocationID, Location, LocationSortOrder, LocationGUID, LocationGUIDVersion, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletSortOrder, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, OutletGUID, OutletGUIDVersion,  OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaOutletCategoryID, OutletCategory, OutletCategorySortOrder)
    SELECT
        l.JobID,
        l.JobEmployeeID,
        l.SurveyorEmployeeID,
        l.SurveyorEmployeeName,
        l.LegionellaID,
        l.BuildingDesignation,
        l.LegionellaStart,
        l.LegionellaFinish,
        l.MonitoringSchedule,
        l.GeneralDescriptionOfSite,
        l.ScopeOfWork,
        l.AreasNotAccessed,
        l.LegionellaNotes,
        l.LegionellaPhotoID,
        l.LegionellaPhotoNo,
        l.LegionellaGUID,
        l.LegionellaGUIDVersion,
        l.LegionellaTypeID,
        l.LegionellaType,
        lo.LegionellaOutletID,
        op.PhotoID [OutletPhotoID],
        op.PhotoNo [OutletPhotoNo],
        lo.SystemRef [OutletSystemRef],
        ll.LegionellaLocationID,
        ll.Location,
        ll.LocationSortOrder,
        ll.LocationGUID,
        ll.LegionellaGUIDVersion,
        lo.Cold [OutletCold],
        lo.Hot [OutletHot],
        lo.Mixed [OutletMixed],
        lo.Mains [OutletMains],
        lo.SentinelCold [OutletSentinelCold],
        lo.SentinelHot [OutletSentinelHot],
        lo.SentinelMixed [OutletSentinelMixed],
        lo.SentinelMains [OutletSentinelMains],
        lo.SortOrder [OutletSortOrder],
        lo.FlushedCold [OutletFlushedCold],
        lo.FlushedHot [OutletFlushedHot],
        lo.FlushedMixed [OutletFlushedMixed],
        lo.FlushedMains [OutletFlushedMains],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        sca.OutletSourceColdAssetID,
        sca.OutletSourceColdAssetSystemRef,
        sca.OutletSourceColdAssetLocation,
        sca.OutletSourceColdAssetCategory,
        sha.OutletSourceHotAssetID,
        sha.OutletSourceHotAssetSystemRef,
        sha.OutletSourceHotAssetLocation,
        sha.OutletSourceHotAssetCategory,
        sma.OutletSourceMixedAssetID,
        sma.OutletSourceMixedAssetSystemRef,
        sma.OutletSourceMixedAssetLocation,
        sma.OutletSourceMixedAssetCategory,
        smna.OutletSourceMainsAssetID,
        smna.OutletSourceMainsAssetSystemRef,
        smna.OutletSourceMainsAssetLocation,
        smna.OutletSourceMainsAssetCategory,
        loc.LegionellaOutletCategoryID,
        loc.Description [OutletCategory],
        loc.SortOrder [OutletCategorySortOrder]
    FROM
        @LegionellaData l
        INNER JOIN @LegionellaLocationData ll ON l.LegionellaID = ll.LegionellaID
        LEFT JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID
        LEFT JOIN LegionellaOutletCategory loc WITH (NOLOCK) ON lo.LegionellaOutletCategoryID = loc.LegionellaOutletCategoryID
        LEFT JOIN Photo op WITH (NOLOCK) ON lo.PhotoID = op.PhotoID AND op.ContentType IS NOT NULL
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceColdAssetID],
                _la.SystemRef [OutletSourceColdAssetSystemRef],
                _la.Location [OutletSourceColdAssetLocation],
                _lac.Description [OutletSourceColdAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceCold
        ) sca -- Source Cold Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceHotAssetID],
                _la.SystemRef [OutletSourceHotAssetSystemRef],
                _la.Location [OutletSourceHotAssetLocation],
                _lac.Description [OutletSourceHotAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceHot
        ) sha -- Source Hot Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceMixedAssetID],
                _la.SystemRef [OutletSourceMixedAssetSystemRef],
                _la.Location [OutletSourceMixedAssetLocation],
                _lac.Description [OutletSourceMixedAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceMixed
        ) sma -- Source Mixed Asset
        OUTER APPLY
        (
            SELECT
                _la.LegionellaAssetID [OutletSourceMainsAssetID],
                _la.SystemRef [OutletSourceMainsAssetSystemRef],
                _la.Location [OutletSourceMainsAssetLocation],
                _lac.Description [OutletSourceMainsAssetCategory]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
                INNER JOIN LegionellaAssetCategory _lac WITH (NOLOCK) ON _la.LegionellaAssetCategoryID = _lac.LegionellaAssetCategoryID
            WHERE
                _la.LegionellaAssetID = lo.SourceMains
        ) smna -- Source Mains Asset
    WHERE
        lo.Deleted IS NULL
            AND
        ( -- Get for the LegionellaOutletID passed in or for all the ones on the job / site.
            lo.LegionellaOutletID = @LegionellaOutletID
                OR
            @LegionellaOutletID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                lo.LegionellaOutletID IS NOT NULL
            )
        )
            AND
        ( -- Get for the Outlet Category passed in.
            @LegionellaOutletCategoryID = 0
                OR
            loc.LegionellaOutletCategoryID = @LegionellaOutletCategoryID
        )
    ORDER BY
        l.LegionellaStart,
        ll.LocationSortOrder,
        lo.SortOrder

    -- Don't insert into @LegionellaQuestionData if we don't need Question data.
    SET NOEXEC OFF;
    IF @IncludeQuestions = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Question data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaQuestionData TABLE (RowID INT IDENTITY(1,1), RowType INT, JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetOutletID INT, AssetOutletPhotoID INT, AssetOutletPhotoNo VARCHAR(50), AssetOutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, AssetOutletLocation VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, AssetOutletSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, AssetOutletGUID VARCHAR(50), AssetOutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaAssetOutletCategoryID INT, AssetOutletCategory VARCHAR(MAX), AssetOutletCategorySortOrder INT, LegionellaAssetOutletQuestionID INT, QuestionSubTabTitle VARCHAR(MAX), QuestionGroupTitle VARCHAR(MAX), Question VARCHAR(MAX), EntryType VARCHAR(MAX), NullReplace VARCHAR(MAX), ReplaceVariable VARCHAR(MAX), LegionellaAssetOutletQuestionSortOrder INT, LegionellaAssetOutletDataCollectionID INT, DataCollectionText VARCHAR(MAX), DataCollectionInt INT, AnswerText VARCHAR(MAX))

    INSERT INTO @LegionellaQuestionData (RowType, JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetOutletID, AssetOutletPhotoID, AssetOutletPhotoNo, AssetOutletSystemRef, LegionellaLocationID, AssetOutletLocation, LocationSortOrder, LocationGUID, LocationGUIDVersion, AssetOutletSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, AssetOutletGUID, AssetOutletGUIDVersion, OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaAssetOutletCategoryID, AssetOutletCategory, AssetOutletCategorySortOrder, LegionellaAssetOutletQuestionID, QuestionSubTabTitle, QuestionGroupTitle, Question, EntryType, NullReplace, ReplaceVariable, LegionellaAssetOutletQuestionSortOrder, LegionellaAssetOutletDataCollectionID, DataCollectionText, DataCollectionInt, AnswerText)
    SELECT
        lao.RowType,
        lao.JobID,
        lao.JobEmployeeID,
        lao.SurveyorEmployeeID,
        lao.SurveyorEmployeeName,
        lao.LegionellaID,
        lao.BuildingDesignation,
        lao.LegionellaStart,
        lao.LegionellaFinish,
        lao.MonitoringSchedule,
        lao.GeneralDescriptionOfSite,
        lao.ScopeOfWork,
        lao.AreasNotAccessed,
        lao.LegionellaNotes,
        lao.LegionellaPhotoID,
        lao.LegionellaPhotoNo,
        lao.LegionellaGUID,
        lao.LegionellaGUIDVersion,
        lao.LegionellaTypeID,
        lao.LegionellaType,
        lao.LegionellaAssetOutletID,
        lao.AssetOutletPhotoID,
        lao.AssetOutletPhotoNo,
        lao.AssetOutletSystemRef,
        lao.LegionellaLocationID,
        lao.AssetOutletLocation,
        lao.LocationSortOrder,
        lao.LocationGUID,
        lao.LocationGUIDVersion,
        lao.AssetOutletSortOrder,
        lao.LegionellaAssetRiskRatingID,
        lao.AssetRiskRating,
        lao.AssetRiskColour,
        lao.AssetRiskRatingSortOrder,
        lao.OutletCold,
        lao.OutletHot,
        lao.OutletMixed,
        lao.OutletMains,
        lao.OutletSentinelCold,
        lao.OutletSentinelHot,
        lao.OutletSentinelMixed,
        lao.OutletSentinelMains,
        lao.OutletFlushedCold,
        lao.OutletFlushedHot,
        lao.OutletFlushedMixed,
        lao.OutletFlushedMains,
        lao.AssetOutletGUID,
        lao.AssetOutletGUIDVersion,
        lao.OutletSourceColdAssetID,
        lao.OutletSourceColdAssetSystemRef,
        lao.OutletSourceColdAssetLocation,
        lao.OutletSourceColdAssetCategory,
        lao.OutletSourceHotAssetID,
        lao.OutletSourceHotAssetSystemRef,
        lao.OutletSourceHotAssetLocation,
        lao.OutletSourceHotAssetCategory,
        lao.OutletSourceMixedAssetID,
        lao.OutletSourceMixedAssetSystemRef,
        lao.OutletSourceMixedAssetLocation,
        lao.OutletSourceMixedAssetCategory,
        lao.OutletSourceMainsAssetID,
        lao.OutletSourceMainsAssetSystemRef,
        lao.OutletSourceMainsAssetLocation,
        lao.OutletSourceMainsAssetCategory,
        lao.LegionellaAssetOutletCategoryID,
        lao.AssetOutletCategory,
        lao.AssetOutletCategorySortOrder,
        laoq.LegionellaAssetOutletQuestionID,
        laoq.SubTabTitle [QuestionSubTabTitle],
        laoq.GroupTitle [QuestionGroupTitle],
        laoq.Description [Question],
        laoq.EntryType,
        laoq.NullReplace,
        laoq.ReplaceVariable,
        laoq.SortOrder [LegionellaAssetOutletQuestionSortOrder],
        laodc.LegionellaAssetOutletDataCollectionID,
        laodc.DataText [DataCollectionText],
        laodc.DataInt [DataCollectionInt],
        COALESCE(NULLIF(dbo.HtmlEncode(laodc.DataText), ''), CAST(laodc.DataInt AS VARCHAR(MAX)), laoq.NullReplace) [AnswerText]
    FROM
        (
            SELECT
                1 [RowType],
                l.JobID,
                l.JobEmployeeID,
                l.SurveyorEmployeeID,
                l.SurveyorEmployeeName,
                l.LegionellaID,
                l.BuildingDesignation,
                l.LegionellaStart,
                l.LegionellaFinish,
                l.MonitoringSchedule,
                l.GeneralDescriptionOfSite,
                l.ScopeOfWork,
                l.AreasNotAccessed,
                l.LegionellaNotes,
                l.LegionellaPhotoID,
                l.LegionellaPhotoNo,
                l.LegionellaGUID,
                l.LegionellaGUIDVersion,
                l.LegionellaTypeID,
                l.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                NULL [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaData l
            WHERE @IncludeLegionella = 1
                UNION ALL
            SELECT
                2 [RowType],
                la.JobID,
                la.JobEmployeeID,
                la.SurveyorEmployeeID,
                la.SurveyorEmployeeName,
                la.LegionellaID,
                la.BuildingDesignation,
                la.LegionellaStart,
                la.LegionellaFinish,
                la.MonitoringSchedule,
                la.GeneralDescriptionOfSite,
                la.ScopeOfWork,
                la.AreasNotAccessed,
                la.LegionellaNotes,
                la.LegionellaPhotoID,
                la.LegionellaPhotoNo,
                la.LegionellaGUID,
                la.LegionellaGUIDVersion,
                la.LegionellaTypeID,
                la.LegionellaType,
                la.LegionellaAssetID [LegionellaAssetOutletID],
                la.AssetPhotoID [AssetOutletPhotoID],
                la.AssetPhotoNo [AssetOutletPhotoNo],
                la.AssetSystemRef [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                la.AssetLocation [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                la.AssetSortOrder [AssetOutletSortOrder],
                la.LegionellaAssetRiskRatingID,
                la.AssetRiskRating,
                la.AssetRiskColour,
                la.AssetRiskRatingSortOrder,
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                la.AssetGUID [AssetOutletGUID],
                la.AssetGUIDVersion [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                la.LegionellaAssetCategoryID [LegionellaAssetOutletCategoryID],
                la.AssetCategory [AssetOutletCategory],
                la.AssetCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaAssetData la
            WHERE @IncludeAssets = 1
                UNION ALL
            SELECT
                3 [RowType],
                ll.JobID,
                ll.JobEmployeeID,
                ll.SurveyorEmployeeID,
                ll.SurveyorEmployeeName,
                ll.LegionellaID,
                ll.BuildingDesignation,
                ll.LegionellaStart,
                ll.LegionellaFinish,
                ll.MonitoringSchedule,
                ll.GeneralDescriptionOfSite,
                ll.ScopeOfWork,
                ll.AreasNotAccessed,
                ll.LegionellaNotes,
                ll.LegionellaPhotoID,
                ll.LegionellaPhotoNo,
                ll.LegionellaGUID,
                ll.LegionellaGUIDVersion,
                ll.LegionellaTypeID,
                ll.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                ll.LegionellaLocationID,
                ll.Location [AssetOutletLocation],
                ll.LocationSortOrder,
                ll.LocationGUID,
                ll.LocationGUIDVersion,
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaLocationData ll
            WHERE @IncludeLocations = 1
                UNION ALL
            SELECT
                4 [RowType],
                lo.JobID,
                lo.JobEmployeeID,
                lo.SurveyorEmployeeID,
                lo.SurveyorEmployeeName,
                lo.LegionellaID,
                lo.BuildingDesignation,
                lo.LegionellaStart,
                lo.LegionellaFinish,
                lo.MonitoringSchedule,
                lo.GeneralDescriptionOfSite,
                lo.ScopeOfWork,
                lo.AreasNotAccessed,
                lo.LegionellaNotes,
                lo.LegionellaPhotoID,
                lo.LegionellaPhotoNo,
                lo.LegionellaGUID,
                lo.LegionellaGUIDVersion,
                lo.LegionellaTypeID,
                lo.LegionellaType,
                lo.LegionellaOutletID [LegionellaAssetOutletID],
                lo.OutletPhotoID [AssetOutletPhotoID],
                lo.OutletPhotoNo [AssetOutletPhotoNo],
                lo.OutletSystemRef [AssetOutletSystemRef],
                lo.LegionellaLocationID,
                lo.Location [AssetOutletLocation],
                lo.LocationSortOrder,
                lo.LocationGUID,
                lo.LocationGUIDVersion,
                lo.OutletSortOrder [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                lo.OutletCold,
                lo.OutletHot,
                lo.OutletMixed,
                lo.OutletMains,
                lo.OutletSentinelCold,
                lo.OutletSentinelHot,
                lo.OutletSentinelMixed,
                lo.OutletSentinelMains,
                lo.OutletFlushedCold,
                lo.OutletFlushedHot,
                lo.OutletFlushedMixed,
                lo.OutletFlushedMains,
                lo.OutletGUID [AssetOutletGUID],
                lo.OutletGUIDVersion [AssetOutletGUIDVersion],
                lo.OutletSourceColdAssetID,
                lo.OutletSourceColdAssetSystemRef,
                lo.OutletSourceColdAssetLocation,
                lo.OutletSourceColdAssetCategory,
                lo.OutletSourceHotAssetID,
                lo.OutletSourceHotAssetSystemRef,
                lo.OutletSourceHotAssetLocation,
                lo.OutletSourceHotAssetCategory,
                lo.OutletSourceMixedAssetID,
                lo.OutletSourceMixedAssetSystemRef,
                lo.OutletSourceMixedAssetLocation,
                lo.OutletSourceMixedAssetCategory,
                lo.OutletSourceMainsAssetID,
                lo.OutletSourceMainsAssetSystemRef,
                lo.OutletSourceMainsAssetLocation,
                lo.OutletSourceMainsAssetCategory,
                lo.LegionellaOutletCategoryID [LegionellaAssetOutletCategoryID],
                lo.OutletCategory [AssetOutletCategory],
                lo.OutletCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaOutletData lo
            WHERE @IncludeOutlets = 1
        ) lao
        INNER JOIN LegionellaAssetOutletQuestion laoq WITH (NOLOCK) ON
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN 1
                WHEN 3 THEN 1
                ELSE lao.LegionellaAssetOutletCategoryID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN 1 -- Legionella questions handled below
                WHEN 2 THEN laoq.LegionellaAssetCategoryID
                WHEN 3 THEN 1 -- Location questions handled below
                WHEN 4 THEN laoq.LegionellaOutletCategoryID
            END
        INNER JOIN LegionellaTypeConfig ltc WITH (NOLOCK) ON laoq.LegionellaAssetOutletQuestionID = ltc.LegionellaAssetOutletQuestionID AND lao.LegionellaTypeID = ltc.LegionellaTypeID
        LEFT JOIN LegionellaAssetOutletDataCollection laodc WITH (NOLOCK) ON laoq.LegionellaAssetOutletQuestionID = laodc.LegionellaAssetOutletQuestionID AND
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN lao.LegionellaID
                WHEN 3 THEN lao.LegionellaLocationID
                ELSE lao.LegionellaAssetOutletID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN laodc.LegionellaID
                WHEN 2 THEN laodc.LegionellaAssetID
                WHEN 3 THEN laodc.LegionellaLocationID
                WHEN 4 THEN laodc.LegionellaOutletID
            END
    WHERE
        laoq.Deleted IS NULL
            AND
        laodc.Deleted IS NULL
            AND
        (
            lao.RowType IN (2, 4) -- Automatically show Asset and Outlet questions.
                OR
            (
                lao.RowType = 1 -- If something from the Legionella record and a question, decide whether to show or hide the Question.
                    AND
                laoq.LegionellaSectionID IS NOT NULL
            )
                OR
            (
                lao.RowType = 3 -- Filter Locations to only show location questions.
                    AND
                laoq.LegionellaSectionID = 4
            )
        )
            AND
        (@LegionellaSectionID IS NULL OR (@LegionellaSectionID IS NOT NULL AND laoq.LegionellaSectionID = @LegionellaSectionID)) -- For a certain Section or not.
            AND
        (@QuestionsHaveAnswers = 0 OR (@QuestionsHaveAnswers = 1 AND laodc.LegionellaAssetOutletDataCollectionID IS NOT NULL)) -- Pull all questions or only ones with answers.
    ORDER BY
        lao.LegionellaStart,
        lao.RowType,
        lao.LocationSortOrder,
        lao.AssetOutletSortOrder,
        laoq.SortOrder

    -- Don't insert into @LegionellaTaskData if we don't need Task data.
    SET NOEXEC OFF;
    IF @IncludeTasks = 0
        BEGIN
            SET NOEXEC ON;
        END

    -- Get Legionella Task data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaTaskData TABLE (RowID INT IDENTITY(1,1), RowType INT, JobID INT, JobEmployeeID INT, SurveyorEmployeeID INT, SurveyorEmployeeName VARCHAR(50), LegionellaID INT, BuildingDesignation VARCHAR(MAX), LegionellaStart DATETIME, LegionellaFinish DATETIME, MonitoringSchedule DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), ScopeOfWork VARCHAR(MAX), AreasNotAccessed VARCHAR(MAX), LegionellaNotes VARCHAR(MAX), LegionellaPhotoID INT, LegionellaPhotoNo VARCHAR(50), LegionellaGUID VARCHAR(50), LegionellaGUIDVersion INT, LegionellaTypeID INT, LegionellaType VARCHAR(1000), LegionellaAssetOutletID INT, AssetOutletPhotoID INT, AssetOutletPhotoNo VARCHAR(50), AssetOutletSystemRef VARCHAR(MAX), LegionellaLocationID INT, AssetOutletLocation VARCHAR(MAX), LocationSortOrder INT, LocationGUID VARCHAR(50), LocationGUIDVersion INT, AssetOutletSortOrder INT, LegionellaAssetRiskRatingID INT, AssetRiskRating VARCHAR(MAX), AssetRiskColour VARCHAR(15), AssetRiskRatingSortOrder INT, OutletCold DECIMAL(10, 2), OutletHot DECIMAL(10, 2), OutletMixed DECIMAL(10, 2), OutletMains DECIMAL(10, 2), OutletSentinelCold INT, OutletSentinelHot INT, OutletSentinelMixed INT, OutletSentinelMains INT, OutletFlushedCold BIT, OutletFlushedHot BIT, OutletFlushedMixed BIT, OutletFlushedMains BIT, AssetOutletGUID VARCHAR(50), AssetOutletGUIDVersion INT, OutletSourceColdAssetID INT, OutletSourceColdAssetSystemRef VARCHAR(MAX), OutletSourceColdAssetLocation VARCHAR(MAX), OutletSourceColdAssetCategory VARCHAR(MAX), OutletSourceHotAssetID INT, OutletSourceHotAssetSystemRef VARCHAR(MAX), OutletSourceHotAssetLocation VARCHAR(MAX), OutletSourceHotAssetCategory VARCHAR(MAX), OutletSourceMixedAssetID INT, OutletSourceMixedAssetSystemRef VARCHAR(MAX), OutletSourceMixedAssetLocation VARCHAR(MAX), OutletSourceMixedAssetCategory VARCHAR(MAX), OutletSourceMainsAssetID INT, OutletSourceMainsAssetSystemRef VARCHAR(MAX), OutletSourceMainsAssetLocation VARCHAR(MAX), OutletSourceMainsAssetCategory VARCHAR(MAX), LegionellaAssetOutletCategoryID INT, AssetOutletCategory VARCHAR(MAX), AssetOutletCategorySortOrder INT, LegionellaTaskID INT, LegionellaTaskNo INT, TaskRiskDescription VARCHAR(MAX), TaskAction VARCHAR(MAX), TaskSortOrder INT, TaskGUID VARCHAR(50), TaskGUIDVersion INT, LegionellaRiskCategoryID INT, RiskCategory VARCHAR(MAX), RiskCategorySortOrder INT, LegionellaFrequencyCategoryID INT, FrequencyCategory VARCHAR(MAX), FrequencyCategoryDateAddModifier VARCHAR(20), FrequencyCategoryDateAddValue INT, FrequencyCategoryDateCalc DATETIME, FrequencyCategorySortOrder INT, LegionellaRiskRatingID INT, RiskRating VARCHAR(MAX), RiskColour VARCHAR(15), RiskRatingSortOrder INT, LegionellaPriorityRatingID INT, PriorityRating VARCHAR(MAX), ShortPriorityRating VARCHAR(20), PriorityColour VARCHAR(15), PriorityRatingSortOrder INT, LegionellaTaskPhotoID INT)

    INSERT INTO @LegionellaTaskData (RowType, JobID, JobEmployeeID, SurveyorEmployeeID, SurveyorEmployeeName, LegionellaID, BuildingDesignation, LegionellaStart, LegionellaFinish, MonitoringSchedule, GeneralDescriptionOfSite, ScopeOfWork, AreasNotAccessed, LegionellaNotes, LegionellaPhotoID, LegionellaPhotoNo, LegionellaGUID, LegionellaGUIDVersion, LegionellaTypeID, LegionellaType, LegionellaAssetOutletID, AssetOutletPhotoID, AssetOutletPhotoNo, AssetOutletSystemRef, LegionellaLocationID, AssetOutletLocation, LocationSortOrder, LocationGUID, LocationGUIDVersion, AssetOutletSortOrder, LegionellaAssetRiskRatingID, AssetRiskRating, AssetRiskColour, AssetRiskRatingSortOrder, OutletCold, OutletHot, OutletMixed, OutletMains, OutletSentinelCold, OutletSentinelHot, OutletSentinelMixed, OutletSentinelMains, OutletFlushedCold, OutletFlushedHot, OutletFlushedMixed, OutletFlushedMains, AssetOutletGUID, AssetOutletGUIDVersion, OutletSourceColdAssetID, OutletSourceColdAssetSystemRef, OutletSourceColdAssetLocation, OutletSourceColdAssetCategory, OutletSourceHotAssetID, OutletSourceHotAssetSystemRef, OutletSourceHotAssetLocation, OutletSourceHotAssetCategory, OutletSourceMixedAssetID, OutletSourceMixedAssetSystemRef, OutletSourceMixedAssetLocation, OutletSourceMixedAssetCategory, OutletSourceMainsAssetID, OutletSourceMainsAssetSystemRef, OutletSourceMainsAssetLocation, OutletSourceMainsAssetCategory, LegionellaAssetOutletCategoryID, AssetOutletCategory, AssetOutletCategorySortOrder, LegionellaTaskID, LegionellaTaskNo, TaskRiskDescription, TaskAction, TaskSortOrder, TaskGUID, TaskGUIDVersion, LegionellaRiskCategoryID, RiskCategory, RiskCategorySortOrder, LegionellaFrequencyCategoryID, FrequencyCategory, FrequencyCategoryDateAddModifier, FrequencyCategoryDateAddValue, FrequencyCategoryDateCalc, FrequencyCategorySortOrder, LegionellaRiskRatingID, RiskRating, RiskColour, RiskRatingSortOrder, LegionellaPriorityRatingID, PriorityRating, ShortPriorityRating, PriorityColour, PriorityRatingSortOrder, LegionellaTaskPhotoID)
    SELECT
        lao.RowType,
        lao.JobID,
        lao.JobEmployeeID,
        lao.SurveyorEmployeeID,
        lao.SurveyorEmployeeName,
        lao.LegionellaID,
        lao.BuildingDesignation,
        lao.LegionellaStart,
        lao.LegionellaFinish,
        lao.MonitoringSchedule,
        lao.GeneralDescriptionOfSite,
        lao.ScopeOfWork,
        lao.AreasNotAccessed,
        lao.LegionellaNotes,
        lao.LegionellaPhotoID,
        lao.LegionellaPhotoNo,
        lao.LegionellaGUID,
        lao.LegionellaGUIDVersion,
        lao.LegionellaTypeID,
        lao.LegionellaType,
        lao.LegionellaAssetOutletID,
        lao.AssetOutletPhotoID,
        lao.AssetOutletPhotoNo,
        lao.AssetOutletSystemRef,
        lao.LegionellaLocationID,
        lao.AssetOutletLocation,
        lao.LocationSortOrder,
        lao.LocationGUID,
        lao.LocationGUIDVersion,
        lao.AssetOutletSortOrder,
        lao.LegionellaAssetRiskRatingID,
        lao.AssetRiskRating,
        lao.AssetRiskColour,
        lao.AssetRiskRatingSortOrder,
        lao.OutletCold,
        lao.OutletHot,
        lao.OutletMixed,
        lao.OutletMains,
        lao.OutletSentinelCold,
        lao.OutletSentinelHot,
        lao.OutletSentinelMixed,
        lao.OutletSentinelMains,
        lao.OutletFlushedCold,
        lao.OutletFlushedHot,
        lao.OutletFlushedMixed,
        lao.OutletFlushedMains,
        lao.AssetOutletGUID,
        lao.AssetOutletGUIDVersion,
        lao.OutletSourceColdAssetID,
        lao.OutletSourceColdAssetSystemRef,
        lao.OutletSourceColdAssetLocation,
        lao.OutletSourceColdAssetCategory,
        lao.OutletSourceHotAssetID,
        lao.OutletSourceHotAssetSystemRef,
        lao.OutletSourceHotAssetLocation,
        lao.OutletSourceHotAssetCategory,
        lao.OutletSourceMixedAssetID,
        lao.OutletSourceMixedAssetSystemRef,
        lao.OutletSourceMixedAssetLocation,
        lao.OutletSourceMixedAssetCategory,
        lao.OutletSourceMainsAssetID,
        lao.OutletSourceMainsAssetSystemRef,
        lao.OutletSourceMainsAssetLocation,
        lao.OutletSourceMainsAssetCategory,
        lao.LegionellaAssetOutletCategoryID,
        lao.AssetOutletCategory,
        lao.AssetOutletCategorySortOrder,
        lt.LegionellaTaskID,
        lt.LegionellaTaskNo,
        lt.RiskDescription [TaskRiskDescription],
        lt.Action [TaskAction],
        lt.SortOrder [TaskSortOrder],
        lt.GUID [TaskGUID],
        lt.GUIDVersion [TaskGUIDVersion],
        lrc.LegionellaRiskCategoryID,
        lrc.Description [RiskCategory],
        lrc.SortOrder [RiskCategorySortOrder],
        lfc.LegionellaFrequencyCategoryID,
        lfc.Description [FrequencyCategory],
        lfc.DateAddModifier [FrequencyCategoryDateAddModifier],
        lfc.DateAddValue [FrequencyCategoryDateAddValue],
        dbo.fn_DateAddFromStringPart(lfc.DateAddModifier, lfc.DateAddValue, lao.MonitoringSchedule) [FrequencyCategoryDateCalc],
        lfc.SortOrder [FrequencyCategorySortOrder],
        lrr.LegionellaRiskRatingID,
        lrr.Description [RiskRating],
        lrr.RiskColour,
        lrr.SortOrder [RiskRatingSortOrder],
        lpr.LegionellaPriorityRatingID,
        lpr.Description [PriorityRating],
        lpr.ShortDescription [ShortPriorityRating],
        lpr.PriorityColour,
        lpr.SortOrder [PriorityRatingSortOrder],
        ltp.PhotoID [LegionellaTaskPhotoID]
    FROM
        (
            SELECT
                1 [RowType],
                l.JobID,
                l.JobEmployeeID,
                l.SurveyorEmployeeID,
                l.SurveyorEmployeeName,
                l.LegionellaID,
                l.BuildingDesignation,
                l.LegionellaStart,
                l.LegionellaFinish,
                l.MonitoringSchedule,
                l.GeneralDescriptionOfSite,
                l.ScopeOfWork,
                l.AreasNotAccessed,
                l.LegionellaNotes,
                l.LegionellaPhotoID,
                l.LegionellaPhotoNo,
                l.LegionellaGUID,
                l.LegionellaGUIDVersion,
                l.LegionellaTypeID,
                l.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                NULL [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaData l
            WHERE @IncludeLegionella = 1
                UNION ALL
            SELECT
                2 [RowType],
                la.JobID,
                la.JobEmployeeID,
                la.SurveyorEmployeeID,
                la.SurveyorEmployeeName,
                la.LegionellaID,
                la.BuildingDesignation,
                la.LegionellaStart,
                la.LegionellaFinish,
                la.MonitoringSchedule,
                la.GeneralDescriptionOfSite,
                la.ScopeOfWork,
                la.AreasNotAccessed,
                la.LegionellaNotes,
                la.LegionellaPhotoID,
                la.LegionellaPhotoNo,
                la.LegionellaGUID,
                la.LegionellaGUIDVersion,
                la.LegionellaTypeID,
                la.LegionellaType,
                la.LegionellaAssetID [LegionellaAssetOutletID],
                la.AssetPhotoID [AssetOutletPhotoID],
                la.AssetPhotoNo [AssetOutletPhotoNo],
                la.AssetSystemRef [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                la.AssetLocation [AssetOutletLocation],
                NULL [LocationSortOrder],
                NULL [LocationGUID],
                NULL [LocationGUIDVersion],
                la.AssetSortOrder [AssetOutletSortOrder],
                la.LegionellaAssetRiskRatingID,
                la.AssetRiskRating,
                la.AssetRiskColour,
                la.AssetRiskRatingSortOrder,
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                la.AssetGUID [AssetOutletGUID],
                la.AssetGUIDVersion [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                la.LegionellaAssetCategoryID [LegionellaAssetOutletCategoryID],
                la.AssetCategory [AssetOutletCategory],
                la.AssetCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaAssetData la
            WHERE @IncludeAssets = 1
                UNION ALL
            SELECT
                3 [RowType],
                ll.JobID,
                ll.JobEmployeeID,
                ll.SurveyorEmployeeID,
                ll.SurveyorEmployeeName,
                ll.LegionellaID,
                ll.BuildingDesignation,
                ll.LegionellaStart,
                ll.LegionellaFinish,
                ll.MonitoringSchedule,
                ll.GeneralDescriptionOfSite,
                ll.ScopeOfWork,
                ll.AreasNotAccessed,
                ll.LegionellaNotes,
                ll.LegionellaPhotoID,
                ll.LegionellaPhotoNo,
                ll.LegionellaGUID,
                ll.LegionellaGUIDVersion,
                ll.LegionellaTypeID,
                ll.LegionellaType,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletPhotoID],
                NULL [AssetOutletPhotoNo],
                NULL [AssetOutletSystemRef],
                ll.LegionellaLocationID,
                ll.Location [AssetOutletLocation],
                ll.LocationSortOrder,
                ll.LocationGUID,
                ll.LocationGUIDVersion,
                NULL [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                NULL [OutletCold],
                NULL [OutletHot],
                NULL [OutletMixed],
                NULL [OutletMains],
                NULL [OutletSentinelCold],
                NULL [OutletSentinelHot],
                NULL [OutletSentinelMixed],
                NULL [OutletSentinelMains],
                NULL [OutletFlushedCold],
                NULL [OutletFlushedHot],
                NULL [OutletFlushedMixed],
                NULL [OutletFlushedMains],
                NULL [AssetOutletGUID],
                NULL [AssetOutletGUIDVersion],
                NULL [OutletSourceColdAssetID],
                NULL [OutletSourceColdAssetSystemRef],
                NULL [OutletSourceColdAssetLocation],
                NULL [OutletSourceColdAssetCategory],
                NULL [OutletSourceHotAssetID],
                NULL [OutletSourceHotAssetSystemRef],
                NULL [OutletSourceHotAssetLocation],
                NULL [OutletSourceHotAssetCategory],
                NULL [OutletSourceMixedAssetID],
                NULL [OutletSourceMixedAssetSystemRef],
                NULL [OutletSourceMixedAssetLocation],
                NULL [OutletSourceMixedAssetCategory],
                NULL [OutletSourceMainsAssetID],
                NULL [OutletSourceMainsAssetSystemRef],
                NULL [OutletSourceMainsAssetLocation],
                NULL [OutletSourceMainsAssetCategory],
                NULL [LegionellaAssetOutletCategoryID],
                NULL [AssetOutletCategory],
                NULL [AssetOutletCategorySortOrder]
            FROM @LegionellaLocationData ll
            WHERE @IncludeLocations = 1
                UNION ALL
            SELECT
                4 [RowType],
                lo.JobID,
                lo.JobEmployeeID,
                lo.SurveyorEmployeeID,
                lo.SurveyorEmployeeName,
                lo.LegionellaID,
                lo.BuildingDesignation,
                lo.LegionellaStart,
                lo.LegionellaFinish,
                lo.MonitoringSchedule,
                lo.GeneralDescriptionOfSite,
                lo.ScopeOfWork,
                lo.AreasNotAccessed,
                lo.LegionellaNotes,
                lo.LegionellaPhotoID,
                lo.LegionellaPhotoNo,
                lo.LegionellaGUID,
                lo.LegionellaGUIDVersion,
                lo.LegionellaTypeID,
                lo.LegionellaType,
                lo.LegionellaOutletID [LegionellaAssetOutletID],
                lo.OutletPhotoID [AssetOutletPhotoID],
                lo.OutletPhotoNo [AssetOutletPhotoNo],
                lo.OutletSystemRef [AssetOutletSystemRef],
                lo.LegionellaLocationID,
                lo.Location [AssetOutletLocation],
                lo.LocationSortOrder,
                lo.LocationGUID,
                lo.LocationGUIDVersion,
                lo.OutletSortOrder [AssetOutletSortOrder],
                NULL [LegionellaAssetRiskRatingID],
                NULL [AssetRiskRating],
                NULL [AssetRiskColour],
                NULL [AssetRiskRatingSortOrder],
                lo.OutletCold,
                lo.OutletHot,
                lo.OutletMixed,
                lo.OutletMains,
                lo.OutletSentinelCold,
                lo.OutletSentinelHot,
                lo.OutletSentinelMixed,
                lo.OutletSentinelMains,
                lo.OutletFlushedCold,
                lo.OutletFlushedHot,
                lo.OutletFlushedMixed,
                lo.OutletFlushedMains,
                lo.OutletGUID [AssetOutletGUID],
                lo.OutletGUIDVersion [AssetOutletGUIDVersion],
                lo.OutletSourceColdAssetID,
                lo.OutletSourceColdAssetSystemRef,
                lo.OutletSourceColdAssetLocation,
                lo.OutletSourceColdAssetCategory,
                lo.OutletSourceHotAssetID,
                lo.OutletSourceHotAssetSystemRef,
                lo.OutletSourceHotAssetLocation,
                lo.OutletSourceHotAssetCategory,
                lo.OutletSourceMixedAssetID,
                lo.OutletSourceMixedAssetSystemRef,
                lo.OutletSourceMixedAssetLocation,
                lo.OutletSourceMixedAssetCategory,
                lo.OutletSourceMainsAssetID,
                lo.OutletSourceMainsAssetSystemRef,
                lo.OutletSourceMainsAssetLocation,
                lo.OutletSourceMainsAssetCategory,
                lo.LegionellaOutletCategoryID [LegionellaAssetOutletCategoryID],
                lo.OutletCategory [AssetOutletCategory],
                lo.OutletCategorySortOrder [AssetOutletCategorySortOrder]
            FROM @LegionellaOutletData lo
            WHERE @IncludeOutlets = 1
        ) lao
        LEFT JOIN LegionellaTask lt WITH (NOLOCK) ON
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN lao.LegionellaID
                WHEN 3 THEN lao.LegionellaLocationID
                ELSE lao.LegionellaAssetOutletID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN lt.LegionellaID
                WHEN 2 THEN lt.LegionellaAssetID
                WHEN 3 THEN lt.LegionellaLocationID
                WHEN 4 THEN lt.LegionellaOutletID
            END
        LEFT JOIN LegionellaRiskCategory lrc WITH (NOLOCK) ON lt.LegionellaRiskCategoryID = lrc.LegionellaRiskCategoryID
        LEFT JOIN LegionellaFrequencyCategory lfc WITH (NOLOCK) ON lt.LegionellaFrequencyCategoryID = lfc.LegionellaFrequencyCategoryID
        LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
        LEFT JOIN LegionellaPriorityRating lpr WITH (NOLOCK) ON lt.LegionellaPriorityRatingID = lpr.LegionellaPriorityRatingID
        OUTER APPLY
        (
            SELECT TOP 1 PhotoID FROM LegionellaTaskPhoto WITH (NOLOCK) WHERE LegionellaTaskID = lt.LegionellaTaskID
        ) ltp
    WHERE
        lt.Deleted IS NULL
            AND
        ( -- Get for the LegionellaTaskID passed in or for all the ones on the job / site.
            lt.LegionellaTaskID = @LegionellaTaskID
                OR
            @LegionellaTaskID = 0
        )
            AND
        (
            -- Act as an INNER JOIN if LegionellaSiteInfoType is not AllItemsSummary.
            @AllItemsSummary = 1
                OR
            (
                @AllItemsSummary = 0
                    AND
                lt.LegionellaTaskID IS NOT NULL
            )
        )
            AND
        (lrr.LegionellaRiskRatingID = @LegionellaRiskRatingID OR @LegionellaRiskRatingID = 0)
    ORDER BY
        lao.LegionellaStart,
        lao.RowType,
        lao.LocationSortOrder,
        lao.AssetOutletSortOrder,
        lt.SortOrder

    /* 3. SELECT FROM THE TEMP TABLES. */
    SET NOEXEC OFF;

    IF @IncludeTasks = 1
    BEGIN
        SELECT * FROM @LegionellaTaskData
    END
    ELSE IF @IncludeQuestions = 1
    BEGIN
        SELECT * FROM @LegionellaQuestionData
    END
    ELSE IF @IncludeOutlets = 1
    BEGIN
        SELECT * FROM @LegionellaOutletData
    END
    ELSE IF @IncludeLocations = 1
    BEGIN
        SELECT * FROM @LegionellaLocationData
    END
    ELSE IF @IncludeAssets = 1
    BEGIN
        SELECT DISTINCT
            0 [JobID],
            lad.JobEmployeeID,
            lad.SurveyorEmployeeID,
            lad.SurveyorEmployeeName,
            lad.LegionellaID,
            lad.BuildingDesignation,
            lad.LegionellaStart,
            lad.LegionellaFinish,
            lad.MonitoringSchedule,
            lad.GeneralDescriptionOfSite,
            lad.ScopeOfWork,
            lad.AreasNotAccessed,
            lad.LegionellaNotes,
            lad.LegionellaPhotoID,
            lad.LegionellaPhotoNo,
            lad.LegionellaGUID,
            lad.LegionellaGUIDVersion,
            lad.LegionellaTypeID,
            lad.LegionellaType,
            lad.LegionellaAssetID,
            lad.AssetPhotoID,
            lad.AssetPhotoNo,
            lad.AssetSystemRef,
            lad.AssetLocation,
            lad.AssetSortOrder,
            lad.LegionellaAssetRiskRatingID,
            lad.AssetRiskRating,
            lad.AssetRiskColour,
            lad.AssetRiskRatingSortOrder,
            lad.AssetGUID,
            lad.AssetGUIDVersion,
            lad.LegionellaAssetCategoryID,
            lad.AssetCategory,
            lad.AssetCategorySortOrder
        FROM
            (
                SELECT -- Get each Legionella Asset record with the max GUID.
                    la.*,
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(la.AssetGUID, NEWID()) ORDER BY la.AssetGUIDVersion DESC) [RowIDForOrder]
                FROM @LegionellaAssetData la
            ) lad
        WHERE lad.RowIDForOrder = 1
    END
    ELSE IF @IncludeLegionella = 1
    BEGIN
        SELECT * FROM @LegionellaData
    END


    SET NOEXEC OFF;
    SET NOCOUNT OFF;
END
GO

-- If one of the Flushed columns does not exist, we assume they all do not exist.
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'FlushedCold' AND object_id = OBJECT_ID('LegionellaOutletPortalData'))
BEGIN
    ALTER TABLE LegionellaOutletPortalData
    ADD [FlushedCold] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD [FlushedHot] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD [FlushedMixed] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD [FlushedMains] [bit] NOT NULL DEFAULT 0

    ALTER TABLE LegionellaOutletComputedData
    ADD [FlushedCold] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD [FlushedHot] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD [FlushedMixed] [bit] NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD [FlushedMains] [bit] NOT NULL DEFAULT 0
END
GO
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'LowUseCold' AND object_id = object_id('LegionellaOutletPortalData'))
BEGIN
    ALTER TABLE LegionellaOutletPortalData
    ADD LowUseCold BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD LowUseHot BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD LowUseMixed BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletPortalData
    ADD LowUseMains BIT NOT NULL DEFAULT 0

    ALTER TABLE LegionellaOutletComputedData
    ADD LowUseCold BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD LowUseHot BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD LowUseMixed BIT NOT NULL DEFAULT 0
    ALTER TABLE LegionellaOutletComputedData
    ADD LowUseMains BIT NOT NULL DEFAULT 0
END
GO
ALTER Proc [dbo].[PopulateLegionellaOutletComputedData]
    @JobID INT
As
Begin
    Set NoCount On;


	DECLARE @SiteID INT = (Select SiteID FROM Job Where JobID=@JobId)
	DECLARE @ClientID INT = (Select ClientID FROM Job Where JobID=@JobId)

	--Clear out the data for the given job
	DELETE LegionellaOutletComputedData Where SiteID = @SiteID

	--Get historic temp data per outlet associated with the job
	DECLARE @LegionellaOutlet TABLE (PerformedByEmployeeID INT,LegionellaOutletID INT, LegionellaLocationID INT, [GUID] VARCHAR(MAX), GUIDVersion INT, LegionellaFinish DATETIME, Cold DECIMAL(10,2), Hot DECIMAL(10,2), [Mixed] DECIMAL(10,2), Mains DECIMAL(10,2), FlushedCold BIT NOT NULL, FlushedHot BIT NOT NULL, FlushedMixed BIT NOT NULL, FlushedMains BIT NOT NULL, LowUseCold BIT NOT NULL, LowUseHot BIT NOT NULL, LowUseMixed BIT NOT NULL, LowUseMains BIT NOT NULL)
	Insert into @LegionellaOutlet (PerformedByEmployeeID,LegionellaOutletID,LegionellaLocationID,[GUID],GUIDVersion,LegionellaFinish,Cold,Hot,[Mixed],Mains, FlushedCold, FlushedHot, FlushedMixed, FlushedMains, LowUseCold, LowUseHot, LowUseMixed, LowUseMains)
	Select
		je.EmployeeID,lo.LegionellaOutletID,lo.LegionellaLocationID,lo.[GUID],lo.GUIDVersion,l.LegionellaFinish,lo.Cold,lo.Hot,lo.[Mixed],lo.Mains,
		ISNULL(lo.FlushedCold, 0) [FlushedCold],
		ISNULL(lo.FlushedHot, 0) [FlushedHot],
		ISNULL(lo.FlushedMixed, 0) [FlushedMixed],
		ISNULL(lo.FlushedMains, 0) [FlushedMains],
		lo.LowUseCold,
		lo.LowUseHot,
		lo.LowUseMixed,
		lo.LowUseMains
	FROM
		Job j WITH (NOLOCK)
		INNER JOIN Quote q WITH (NOLOCK) ON q.JobID=j.JobID
		INNER JOIN Appointment a WITH (NOLOCK) ON a.QuoteID=q.QuoteID AND a.DateDeclined IS NULL
		INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID=je.JobID
		INNER JOIN Legionella l WITH (NOLOCK) ON l.JobEmployeeID=je.JobEmployeeID
		INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON ll.LegionellaID=l.LegionellaID AND ll.Deleted IS NULL
		INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON lo.LegionellaLocationID=ll.LegionellaLocationID AND lo.Deleted IS NULL
	Where
		j.SiteID=@SiteID
			AND
		j.ClientID=@ClientID
			AND
		(
			j.Approved IS NOT NULL
				OR
			j.JobID = @JobID
		)
	Group By
		je.EmployeeID,lo.LegionellaOutletID,lo.LegionellaLocationID,lo.[GUID],lo.GUIDVersion,l.LegionellaFinish,lo.Cold,lo.Hot,lo.[Mixed],lo.Mains,
		lo.FlushedCold,
		lo.FlushedHot,
		lo.FlushedMixed,
		lo.FlushedMains,
		lo.LowUseCold,
		lo.LowUseHot,
		lo.LowUseMixed,
		lo.LowUseMains

	DECLARE @LegionellaOutletComputedData TABLE (JobID INT, OriginalOutletID INT,LegionellaOutletID INT, LegionellaLocationID INT, Cold DECIMAL(10,2), Hot DECIMAL(10,2), [Mixed] DECIMAL(10,2), Mains DECIMAL(10,2), PerformedByEmployeeID INT, PerformedByPortalUserID INT, PerformedByThirdParty VARCHAR(MAX), Comments VARCHAR(MAX), Recorded DATETIME, FlushedCold BIT NOT NULL, FlushedHot BIT NOT NULL, FlushedMixed BIT NOT NULL, FlushedMains BIT NOT NULL, LowUseCold BIT NOT NULL, LowUseHot BIT NOT NULL, LowUseMixed BIT NOT NULL, LowUseMains BIT NOT NULL)
	INSERT INTO @LegionellaOutletComputedData (JobID,OriginalOutletID,LegionellaOutletID,LegionellaLocationID,Cold,Hot,[Mixed],Mains,PerformedByEmployeeID,PerformedByPortalUserID,PerformedByThirdParty,Comments,Recorded, FlushedCold, FlushedHot, FlushedMixed, FlushedMains, LowUseCold, LowUseHot, LowUseMixed, LowUseMains)
	Select
		main.JobID, main.LegionellaOutletID [OriginalOutletID],main.CurrentOutletID [LegionellaOutletID], main.CurrentLocationID [LegionellaLocationID],main.Cold,main.Hot,main.[Mixed],main.Mains,main.PerformedByEmployeeID,main.PerformedByPortalUserID,main.PerformedByThirdParty,main.Comments,main.Recorded, main.FlushedCold, main.FlushedHot, main.FlushedMixed, main.FlushedMains, main.LowUseCold, main.LowUseHot, main.LowUseMixed, main.LowUseMains
	FROM
		(
			Select
				@JobID [JobID], 
				lo.LegionellaOutletID [CurrentOutletID],
				lo.LegionellaLocationID [CurrentLocationID],
				ROW_NUMBER() OVER (Partition By loMax.GUID,loMax.LegionellaFinish Order by lo.LegionellaFinish DESC) [Identifier],
				loMax.LegionellaOutletID,
				loMax.[GUID],
				loMax.Cold,
				loMax.Hot,
				loMax.Mixed,
				loMax.Mains,
				lo.PerformedByEmployeeID,
				NULL [PerformedByPortalUserID],
				NULL [PerformedByThirdParty],
				NULL [Comments],
				loMax.LegionellaFinish [Recorded],
				loMax.FlushedCold,
				loMax.FlushedHot,
				loMax.FlushedMixed,
				loMax.FlushedMains,
				loMax.LowUseCold,
				loMax.LowUseHot,
				loMax.LowUseMixed,
				loMax.LowUseMains
			FROM 
				@LegionellaOutlet lo
				INNER JOIN @LegionellaOutlet loMax ON lo.GUID=loMax.GUID
		) main
	Where
		main.Identifier = 1

	INSERT INTO @LegionellaOutletComputedData (JobID,OriginalOutletID,LegionellaOutletID,LegionellaLocationID,Cold,Hot,[Mixed],Mains,PerformedByEmployeeID,PerformedByPortalUserID,PerformedByThirdParty,Comments,Recorded, FlushedCold, FlushedHot, FlushedMixed, FlushedMains, LowUseCold, LowUseHot, LowUseMixed, LowUseMains)
	SELECT
		locd.JobID,locd.OriginalOutletID,locd.LegionellaOutletID,locd.LegionellaLocationID,lopd.Cold,lopd.Hot,lopd.[Mixed],lopd.Mains,NULL [PerformedByEmployeeID],lopd.PerformedByPortalUserID,lopd.PerformedByThirdParty,lopd.Comments,lopd.Recorded, lopd.FlushedCold, lopd.FlushedHot, lopd.FlushedMixed, lopd.FlushedMains, lopd.LowUseCold, lopd.LowUseHot, lopd.LowUseMixed, lopd.LowUseMains
	FROM
		@LegionellaOutletComputedData locd
		INNER JOIN LegionellaOutletPortalData lopd ON locd.OriginalOutletID = lopd.LegionellaOutletID

	Insert into LegionellaOutletComputedData (ClientID,SiteID,JobID,LegionellaOutletID,LegionellaLocationID,Cold,Hot,Mixed,mains,PerformedByEmployeeID,PerformedByPortalUserID,PerformedByThirdParty,Comments,Recorded, FlushedCold, FlushedHot, FlushedMixed, FlushedMains, LowUseCold, LowUseHot, LowUseMixed, LowUseMains)
	Select
		@ClientID,@SiteID,JobID,LegionellaOutletID,LegionellaLocationID,Cold,Hot,Mixed,mains,PerformedByEmployeeID,PerformedByPortalUserID,PerformedByThirdParty,Comments,Recorded, FlushedCold, FlushedHot, FlushedMixed, FlushedMains, LowUseCold, LowUseHot, LowUseMixed, LowUseMains
	FROM
		@LegionellaOutletComputedData


    Set NoCount Off;
End
GO

-- If one of the new chart columns does not exist, we assume they all do not exist.
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'b__DisplayPortalLegLowUseOutletsChart' AND object_id = OBJECT_ID('Config'))
BEGIN
    ALTER TABLE Config
    ADD b__DisplayPortalLegLowUseOutletsChart BIT NOT NULL DEFAULT 0
    ALTER TABLE Config
    ADD b__DisplayPortalLegOutletsOutOfSpecChart BIT NOT NULL DEFAULT 0

    ALTER TABLE PortalUser
    ADD LegLowUseOutletsChart BIT NOT NULL DEFAULT 0
    ALTER TABLE PortalUser
    ADD LegOutletsOutOfSpecChart BIT NOT NULL DEFAULT 0
END
GO
IF NOT EXISTS(SELECT * FROM SiteLegionellaTemperatureLimits WHERE SiteID = -1) -- Create the default temperatures.
BEGIN
    INSERT INTO SiteLegionellaTemperatureLimits (SiteID, ColdMin, ColdMax, HotMin, HotMax, MixedMin, MixedMax, MainsMin, MainsMax)
    VALUES (-1, 0, 20, 50, 62, 37, 45, 0, 20)
END
GO
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaHomeTabGraphData')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaHomeTabGraphData] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaHomeTabGraphData]
    @PortalUserID INT = NULL,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @ReturnAsChart BIT = NULL,
    @TotalCompliance BIT = 1,
    @RiskItems BIT = 1,
    @LowUseOutlets BIT = 0,
    @OutletsOutOfSpec BIT = 0,
    @RiskItemID INT = 0,
    @OutOfSpec BIT = 0,
    @TaskPriorities bit = 0,
    @PriorityRatingID int = 0
/**********************************************************************
** Overview: Get Legionella data which is used by the graphs on the Home tab of the Portal.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
				AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
				AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get all Total Compliance data up front to reduce table scans (get the most recent job for each Site).
    CREATE TABLE #TotalComplianceData (SiteID INT PRIMARY KEY, Post2000 BIT NOT NULL, UnmanagedSiteLeg BIT NOT NULL, JobID INT)

    -- Add an index on important #TotalComplianceData fields to increase speed below.
    CREATE INDEX temp_TotalComplianceData ON #TotalComplianceData (SiteID, JobID)

    -- Get Total Compliance data for Sites with a Survey.
    IF @TotalCompliance = 1 OR @RiskItems = 1 OR @LowUseOutlets = 1 OR @OutletsOutOfSpec = 1 OR @TaskPriorities = 1
    BEGIN
        INSERT INTO #TotalComplianceData (SiteID, Post2000, UnmanagedSiteLeg, JobID)
        SELECT SiteID, Post2000, UnmanagedSiteLeg, JobID
        FROM
        (
            SELECT
                si.SiteID,
                si.Post2000,
                si.UnmanagedSiteLeg,
                CASE WHEN si.UnmanagedSiteLeg = 0 THEN ISNULL(jd.JobID, -1) ELSE NULL END [JobID],
                ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.LegionellaFinish DESC, jd.JobID DESC) [RowID]
            FROM
                @ClientSiteData csd
                INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
                INNER JOIN (
                    SELECT
                        a.*,
                        ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.LegionellaFinish DESC, a.JobID DESC) [RowID]
                    FROM
                    (
                        SELECT 0 [IsSiteDocument], j.ClientID, j.SiteID, j.JobID, l.LegionellaFinish
                        FROM
                            Job j WITH (NOLOCK)
                            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
                            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
                            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
                            INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
                        WHERE j.Approved IS NOT NULL AND j.Cancelled IS NULL
                        UNION ALL
                        SELECT 1 [IsSiteDocument], NULL [ClientID], sid.SiteID, NULL [JobID], sidi.WorkDate [LegionellaFinish]
                        FROM
                            SiteDocument sid WITH (NOLOCK)
                            INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentId = sidi.SiteDocumentID
                        WHERE
                            sid.SiteDocumentTypeID = 6 -- Legionella Surveys
                                AND
                            sid.Deleted IS NULL
                    ) a
                ) jd ON
                    CASE WHEN jd.IsSiteDocument = 1
                        THEN -1
                        ELSE csd.ClientID
                    END = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
			WHERE
				si.UnmanagedSiteLeg = 0
            GROUP BY
                si.SiteID,
                si.Post2000,
                si.UnmanagedSiteLeg,
                jd.IsSiteDocument,
                jd.JobID,
                jd.LegionellaFinish,
                jd.RowID
        ) a
        WHERE a.RowID = 1
        GROUP BY
            a.SiteID,
            a.Post2000,
            a.UnmanagedSiteLeg,
            a.JobID
        ORDER BY
            a.SiteID
    END

    -- Get Legionella Items data up front to reduce table scans on the Legionella/Asset/Location/Outlet tables.
    DECLARE @LegionellaItemsData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, RowType INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, DateApproved DATETIME, LegionellaAssetOutletID INT, AssetOutletGUID VARCHAR(MAX), AssetOutletGUIDVersion INT, LegionellaLocationID INT, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT,  OutletEnabledCold BIT, OutletEnabledHot BIT, OutletEnabledMixed BIT, OutletEnabledMains BIT, OutletLowUse BIT)

    IF @RiskItems = 1 OR @LowUseOutlets = 1 OR @OutletsOutOfSpec = 1 OR @TaskPriorities = 1
    BEGIN
        INSERT INTO @LegionellaItemsData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, DateApproved, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, OutletEnabledCold, OutletEnabledHot, OutletEnabledMixed, OutletEnabledMains, OutletLowUse)
        SELECT
            tcd.SiteID,
            j.JobID,
            lao.RowType,
            l.LegionellaID,
            l.GUID [LegGUID],
            l.GUIDVersion [LegGUIDVersion],
            l.DateApproved,
            lao.LegionellaAssetOutletID,
            lao.AssetOutletGUID,
            lao.AssetOutletGUIDVersion,
            lao.LegionellaLocationID,
            lao.LocationGUID,
            lao.LocationGUIDVersion,
            lao.OutletEnabledCold,
            lao.OutletEnabledHot,
            lao.OutletEnabledMixed,
            lao.OutletEnabledMains,
            CASE WHEN (lao.OutletEnabledCold = 1 AND lao.OutletLowUseCold = 1) OR (lao.OutletEnabledHot = 1 AND lao.OutletLowUseHot = 1) OR (lao.OutletEnabledMixed = 1 AND lao.OutletLowUseMixed = 1) OR (lao.OutletEnabledMains = 1 AND lao.OutletLowUseMains = 1)
                THEN 1
                ELSE 0
            END [OutletLowUse]
        FROM
            #TotalComplianceData tcd
            INNER JOIN Job j WITH (NOLOCK) ON tcd.SiteID = j.SiteID AND j.Approved IS NOT NULL AND j.Cancelled IS NULL
            INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID
            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID
            INNER JOIN (
                SELECT
                    1 [RowType],
                    l.LegionellaID,
                    NULL [LegionellaAssetOutletID],
                    NULL [AssetOutletSystemRef],
                    NULL [AssetOutletGUID],
                    NULL [AssetOutletGUIDVersion],
                    NULL [LegionellaLocationID],
                    NULL [AssetOutletLocation],
                    NULL [LocationGUID],
                    NULL [LocationGUIDVersion],
                    NULL [OutletEnabledCold],
                    NULL [OutletEnabledHot],
                    NULL [OutletEnabledMixed],
                    NULL [OutletEnabledMains],
                    NULL [OutletLowUseCold],
                    NULL [OutletLowUseHot],
                    NULL [OutletLowUseMixed],
                    NULL [OutletLowUseMains]
                FROM
                    Legionella l WITH (NOLOCK)

                UNION ALL

                SELECT
                    2 [RowType],
                    la.LegionellaID,
                    la.LegionellaAssetID [LegionellaAssetOutletID],
                    la.SystemRef [AssetOutletSystemRef],
                    la.GUID [AssetOutletGUID],
                    la.GUIDVersion [AssetOutletGUIDVersion],
                    NULL [LegionellaLocationID],
                    la.Location [AssetOutletLocation],
                    NULL [LocationGUID],
                    NULL [LocationGUIDVersion],
                    NULL [OutletEnabledCold],
                    NULL [OutletEnabledHot],
                    NULL [OutletEnabledMixed],
                    NULL [OutletEnabledMains],
                    NULL [OutletLowUseCold],
                    NULL [OutletLowUseHot],
                    NULL [OutletLowUseMixed],
                    NULL [OutletLowUseMains]
                FROM
                    LegionellaAsset la WITH (NOLOCK)
                WHERE
                    la.Deleted IS NULL

                UNION ALL

                SELECT
                    3 [RowType],
                    ll.LegionellaID,
                    NULL [LegionellaAssetOutletID],
                    NULL [AssetOutletSystemRef],
                    NULL [AssetOutletGUID],
                    NULL [AssetOutletGUIDVersion],
                    ll.LegionellaLocationID,
                    ll.Location [AssetOutletLocation],
                    ll.GUID [LocationGUID],
                    ll.GUIDVersion [LocationGUIDVersion],
                    NULL [OutletEnabledCold],
                    NULL [OutletEnabledHot],
                    NULL [OutletEnabledMixed],
                    NULL [OutletEnabledMains],
                    NULL [OutletLowUseCold],
                    NULL [OutletLowUseHot],
                    NULL [OutletLowUseMixed],
                    NULL [OutletLowUseMains]
                FROM
                    LegionellaLocation ll WITH (NOLOCK)
                WHERE
                    ll.Deleted IS NULL

                UNION ALL

                SELECT
                    4 [RowType],
                    ll.LegionellaID,
                    lo.LegionellaOutletID [LegionellaAssetOutletID],
                    lo.SystemRef [AssetOutletSystemRef],
                    lo.GUID [AssetOutletGUID],
                    lo.GUIDVersion [AssetOutletGUIDVersion],
                    ll.LegionellaLocationID,
                    ll.Location [AssetOutletLocation],
                    ll.GUID [LocationGUID],
                    ll.GUIDVersion [LocationGUIDVersion],
                    lo.EnabledCold [OutletEnabledCold],
                    lo.EnabledHot [OutletEnabledHot],
                    lo.EnabledMixed [OutletEnabledMixed],
                    lo.EnabledMains [OutletEnabledMains],
                    lo.LowUseCold [OutletLowUseCold],
                    lo.LowUseHot [OutletLowUseHot],
                    lo.LowUseMixed [OutletLowUseMixed],
                    lo.LowUseMains [OutletLowUseMains]
                FROM
                    LegionellaLocation ll WITH (NOLOCK)
                    INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID AND lo.Deleted IS NULL
                WHERE
                    ll.Deleted IS NULL
            ) lao ON l.LegionellaID = lao.LegionellaID
        GROUP BY
            tcd.SiteID,
            j.JobID,
            lao.RowType,
            l.LegionellaID,
            l.GUID,
            l.GUIDVersion,
            l.DateApproved,
            lao.LegionellaAssetOutletID,
            lao.AssetOutletGUID,
            lao.AssetOutletGUIDVersion,
            lao.LegionellaLocationID,
            lao.LocationGUID,
            lao.LocationGUIDVersion,
            lao.OutletEnabledCold,
            lao.OutletEnabledHot,
            lao.OutletEnabledMixed,
            lao.OutletEnabledMains,
            lao.OutletLowUseCold,
            lao.OutletLowUseHot,
            lao.OutletLowUseMixed,
            lao.OutletLowUseMains
    END

    -- Get Legionella data combined as GUIDs up front as used below.
    /*DECLARE @LegionellaGUIDData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT)

    IF @RiskItems = 1 OR @LowUseOutlets = 1 OR @OutletsOutOfSpec = 1
    BEGIN
        INSERT INTO @LegionellaGUIDData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion)
        SELECT
            l.SiteID,
            l.JobID,
            l.LegionellaID,
            l.LegGUID,
            l.LegGUIDVersion
        FROM
            (
                SELECT -- Get each Legionella record with the max GUID.
                    l.SiteID,
                    l.JobID,
                    l.LegionellaID,
                    l.LegGUID,
                    l.LegGUIDVersion,
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(l.LegGUID, NEWID()) ORDER BY l.LegGUIDVersion DESC, l.DateApproved DESC) [RowID]
                FROM @LegionellaItemsData l
                WHERE l.RowType = 1
            ) l
        WHERE l.RowID = 1
    END*/

    -- Get Legionella Asset data combined as GUIDs up front as used below.
    /*DECLARE @LegionellaAssetGUIDData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaAssetID INT, AssetGUID VARCHAR(MAX), AssetGUIDVersion INT)

    IF @RiskItems = 1
    BEGIN
        INSERT INTO @LegionellaAssetGUIDData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetID, AssetGUID, AssetGUIDVersion)
        SELECT
            la.SiteID,
            la.JobID,
            la.LegionellaID,
            la.LegGUID,
            la.LegGUIDVersion,
            la.LegionellaAssetID,
            la.AssetGUID,
            la.AssetGUIDVersion
        FROM
            (
                SELECT -- Get each Legionella Asset record with the max GUID.
                    la.SiteID,
                    la.JobID,
                    la.LegionellaID,
                    la.LegGUID,
                    la.LegGUIDVersion,
                    la.LegionellaAssetOutletID [LegionellaAssetID],
                    la.AssetOutletGUID [AssetGUID],
                    la.AssetOutletGUIDVersion [AssetGUIDVersion],
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(la.AssetOutletGUID, NEWID()) ORDER BY la.AssetOutletGUIDVersion DESC) [RowID]
                FROM @LegionellaItemsData la
                WHERE la.RowType = 2
            ) la
        WHERE la.RowID = 1
    END*/

    -- Get Legionella Location data combined as GUIDs up front as used below.
    /*DECLARE @LegionellaLocationGUIDData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaLocationID INT NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT)

    IF @RiskItems = 1 OR @LowUseOutlets = 1 OR @OutletsOutOfSpec = 1
    BEGIN
        INSERT INTO @LegionellaLocationGUIDData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion)
        SELECT
            ll.SiteID,
            ll.JobID,
            ll.LegionellaID,
            ll.LegGUID,
            ll.LegGUIDVersion,
            ll.LegionellaLocationID,
            ll.LocationGUID,
            ll.LegGUIDVersion
        FROM
            (
                SELECT -- Get each Legionella Location record with the max GUID.
                    ll.SiteID,
                    ll.JobID,
                    ll.LegionellaID,
                    ll.LegGUID,
                    ll.LegGUIDVersion,
                    ll.LegionellaLocationID,
                    ll.LocationGUID,
                    ll.LocationGUIDVersion,
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(ll.LocationGUID, NEWID()) ORDER BY ll.LocationGUIDVersion DESC) [RowID]
                FROM @LegionellaItemsData ll
                WHERE ll.RowType = 3
            ) ll
        WHERE ll.RowID = 1
    END*/

    -- Get Legionella Outlet data combined as GUIDs up front as used below. As well as getting the latest GUID version, get the latest LegionellaOutletComputedData version (if available).
    DECLARE @LegionellaOutletGUIDData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaLocationID INT NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaOutletID INT NOT NULL, OutletGUID VARCHAR(MAX), OutletGUIDVersion INT, Cold DECIMAL(10, 2), Hot DECIMAL(10, 2), Mixed DECIMAL(10, 2), Mains DECIMAL(10, 2), Flushed BIT NOT NULL, LowUse BIT NOT NULL)

    IF @RiskItems = 1 OR @LowUseOutlets = 1 OR @OutletsOutOfSpec = 1
    BEGIN
        INSERT INTO @LegionellaOutletGUIDData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaOutletID, OutletGUID, OutletGUIDVersion, Cold, Hot, Mixed, Mains, Flushed, LowUse)
        SELECT
            lo.SiteID,
            lo.JobID,
            lo.LegionellaID,
            lo.LegGUID,
            lo.LegGUIDVersion,
            lo.LegionellaLocationID,
            lo.LocationGUID,
            lo.LocationGUIDVersion,
            lo.LegionellaOutletID,
            lo.OutletGUID,
            lo.OutletGUIDVersion,
            lo.Cold,
            lo.Hot,
            lo.Mixed,
            lo.Mains,
            lo.Flushed,
            lo.LowUse
        FROM
            (
                SELECT -- Get each Legionella Outlet record with the max GUID.
                    lo.SiteID,
                    lo.JobID,
                    lo.LegionellaID,
                    lo.LegGUID,
                    lo.LegGUIDVersion,
                    lo.LegionellaLocationID,
                    lo.LocationGUID,
                    lo.LocationGUIDVersion,
                    lo.LegionellaOutletID,
                    lo.OutletGUID,
                    lo.OutletGUIDVersion,
                    CASE WHEN lo.EnabledCold = 1 THEN locd.Cold ELSE NULL END [Cold],
                    CASE WHEN lo.EnabledHot = 1 THEN locd.Hot ELSE NULL END [Hot],
                    CASE WHEN lo.EnabledMixed = 1 THEN locd.Mixed ELSE NULL END [Mixed],
                    CASE WHEN lo.EnabledMains = 1 THEN locd.Mains ELSE NULL END [Mains],
                    CASE WHEN (lo.EnabledCold = 1 AND locd.FlushedCold = 1) OR (lo.EnabledHot = 1 AND locd.FlushedHot = 1) OR (lo.EnabledMixed = 1 AND locd.FlushedMixed = 1) OR (lo.EnabledMains = 1 AND locd.FlushedMains = 1)
                        THEN 1
                        ELSE 0
                    END [Flushed],
                    lo.OutletLowUse [LowUse],
                    ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.OutletGUID, NEWID()) ORDER BY lo.OutletGUIDVersion DESC, locd.Created DESC) [RowID]
                FROM
                    (
                        SELECT -- Get each Legionella Outlet record with the max GUID.
                            lo.SiteID,
                            lo.JobID,
                            lo.LegionellaID,
                            lo.LegGUID,
                            lo.LegGUIDVersion,
                            lo.LegionellaLocationID,
                            lo.LocationGUID,
                            lo.LocationGUIDVersion,
                            lo.LegionellaAssetOutletID [LegionellaOutletID],
                            lo.AssetOutletGUID [OutletGUID],
                            lo.AssetOutletGUIDVersion [OutletGUIDVersion],
                            lo.OutletEnabledCold [EnabledCold],
                            lo.OutletEnabledHot [EnabledHot],
                            lo.OutletEnabledMixed [EnabledMixed],
                            lo.OutletEnabledMains [EnabledMains],
                            lo.OutletLowUse,
                            ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.AssetOutletGUID, NEWID()) ORDER BY lo.AssetOutletGUIDVersion DESC) [RowID]
                        FROM @LegionellaItemsData lo
                        WHERE lo.RowType = 4
                    ) lo
                    INNER JOIN LegionellaOutletComputedData locd WITH (NOLOCK) ON lo.LegionellaOutletID = locd.LegionellaOutletID
                    WHERE lo.RowID = 1
            ) lo
        WHERE lo.RowID = 1
    END

    -- Get Legionella Task up front to reduce table scans on the LegionellaTask table.
    -- NOTE: Columns are out of order with the LegionellaTask table, as some of these we only get when viewing the data from a popup window. This is to increase speed when the data is not needed.
    DECLARE @LegionellaTasksData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, RowType INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaAssetOutletID INT, AssetOutletGUID VARCHAR(MAX), AssetOutletGUIDVersion INT, LegionellaLocationID INT, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaTaskID INT NOT NULL, TaskGUID VARCHAR(MAX), TaskGUIDVersion INT, LegionellaRiskRatingID INT, /* ADDITIONAL COLUMNS */ RiskDescription VARCHAR(MAX), Action VARCHAR(MAX), LegionellaFrequencyCategoryID INT, LegionellaPriorityRatingID INT)

    IF @RiskItems = 1 OR @TaskPriorities = 1
    BEGIN
        IF @ReturnAsChart = 1 -- Basic columns only.
        BEGIN
            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
				lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaID = lt.LegionellaID AND lt.Deleted IS NULL AND lao.RowType = 1
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
				lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaAssetOutletID = lt.LegionellaAssetID AND lt.Deleted IS NULL AND lao.LegionellaLocationID IS NULL AND lao.RowType = 2
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
				lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaLocationID = lt.LegionellaLocationID AND lt.Deleted IS NULL AND lao.LegionellaLocationID IS NOT NULL AND lao.RowType = 3
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
				lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaAssetOutletID = lt.LegionellaOutletID AND lt.Deleted IS NULL AND lao.RowType = 4
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
        END
        ELSE
        BEGIN -- Additional columns included.
            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskDescription, Action, LegionellaFrequencyCategoryID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
                lt.RiskDescription,
                lt.Action,
                lt.LegionellaFrequencyCategoryID,
                lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaID = lt.LegionellaID AND lt.Deleted IS NULL AND lao.RowType=1
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskDescription, Action, LegionellaFrequencyCategoryID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
                lt.RiskDescription,
                lt.Action,
                lt.LegionellaFrequencyCategoryID,
                lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaAssetOutletID = lt.LegionellaAssetID AND lt.Deleted IS NULL AND lao.LegionellaLocationID IS NULL AND lao.RowType=2
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskDescription, Action, LegionellaFrequencyCategoryID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
                lt.RiskDescription,
                lt.Action,
                lt.LegionellaFrequencyCategoryID,
                lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaLocationID = lt.LegionellaLocationID AND lt.Deleted IS NULL AND lao.LegionellaLocationID IS NOT NULL AND lao.RowType=3
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1

            INSERT INTO @LegionellaTasksData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskDescription, Action, LegionellaFrequencyCategoryID, LegionellaPriorityRatingID)
            SELECT
                lao.SiteID,
                lao.JobID,
                lao.RowType,
                lao.LegionellaID,
                lao.LegGUID,
                lao.LegGUIDVersion,
                lao.LegionellaAssetOutletID,
                lao.AssetOutletGUID,
                lao.AssetOutletGUIDVersion,
                lao.LegionellaLocationID,
                lao.LocationGUID,
                lao.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.GUID [TaskGUID],
                lt.GUIDVersion [TaskGUIDVersion],
                lt.LegionellaRiskRatingID,
                lt.RiskDescription,
                lt.Action,
                lt.LegionellaFrequencyCategoryID,
                lt.LegionellaPriorityRatingID
            FROM
                @LegionellaItemsData lao
                INNER JOIN LegionellaTask lt WITH (NOLOCK) ON lao.LegionellaAssetOutletID = lt.LegionellaOutletID AND lt.Deleted IS NULL AND lao.RowType=4
            WHERE
                lt.Deleted IS NULL
                    AND
                CASE WHEN @RiskItemID = 0 -- Risk filter.
                    THEN 1
                    ELSE
                        CASE WHEN @RiskItemID = -1
                            THEN CASE WHEN lt.LegionellaRiskRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @RiskItemID = ISNULL(lt.LegionellaRiskRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
                    AND
                CASE WHEN @PriorityRatingID = 0 -- Priority filter.
                    THEN 1
                    ELSE
                        CASE WHEN @PriorityRatingID = -1
                            THEN CASE WHEN lt.LegionellaPriorityRatingID IS NULL THEN 1 ELSE 0 END
                            ELSE CASE WHEN @PriorityRatingID = ISNULL(lt.LegionellaPriorityRatingID, -1) THEN 1 ELSE 0 END
                        END
                END = 1
        END
    END

    -- Get Legionella Risk Items data up front as used below. Basically GUID Tasks.
    -- NOTE: Columns are out of order with the LegionellaTask table, as some of these we only get when viewing the data from a popup window. This is to increase speed when the data is not needed.
    DECLARE @LegionellaRiskItemsData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, RowType INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaAssetOutletID INT, AssetOutletGUID VARCHAR(MAX), AssetOutletGUIDVersion INT, LegionellaLocationID INT, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaTaskID INT NOT NULL, TaskGUID VARCHAR(MAX), TaskGUIDVersion INT, LegionellaRiskRatingID INT, RiskRating VARCHAR(MAX), RiskColour VARCHAR(15), RiskRatingSortOrder INT, /* ADDITIONAL COLUMNS */ JobNo INT, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), BuildingDesignation VARCHAR(50), AssetOutletSystemRef VARCHAR(8000), Location VARCHAR(8000), RiskDescription VARCHAR(MAX), Action VARCHAR(MAX), LegionellaFrequencyCategoryID INT, FrequencyCategory VARCHAR(8000), LegionellaPriorityRatingID INT, PriorityRating VARCHAR(20), PriorityColour VARCHAR(15))

    IF @RiskItems = 1
    BEGIN
        IF @ReturnAsChart = 1 -- Basic columns only.
        BEGIN
            INSERT INTO @LegionellaRiskItemsData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskRating, RiskColour, RiskRatingSortOrder)
            SELECT
                lt.SiteID,
                lt.JobID,
                lt.RowType,
                lt.LegionellaID,
                lt.LegGUID,
                lt.LegGUIDVersion,
                lt.LegionellaAssetOutletID,
                lt.AssetOutletGUID,
                lt.AssetOutletGUIDVersion,
                lt.LegionellaLocationID,
                lt.LocationGUID,
                lt.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.TaskGUID,
                lt.TaskGUIDVersion,
                lrr.LegionellaRiskRatingID,
                lrr.Description [RiskRating],
                lrr.RiskColour,
                lrr.SortOrder [RiskRatingSortOrder]
            FROM
                (
                    SELECT -- Get each Legionella Task record with the max GUID.
                        lt.SiteID,
                        lt.JobID,
                        lt.RowType,
                        lt.LegionellaID,
                        lt.LegGUID,
                        lt.LegGUIDVersion,
                        lt.LegionellaAssetOutletID,
                        lt.AssetOutletGUID,
                        lt.AssetOutletGUIDVersion,
                        lt.LegionellaLocationID,
                        lt.LocationGUID,
                        lt.LocationGUIDVersion,
                        lt.LegionellaTaskID,
                        lt.TaskGUID,
                        lt.TaskGUIDVersion,
                        lt.LegionellaRiskRatingID,
                        ROW_NUMBER() OVER (PARTITION BY ISNULL(lt.TaskGUID, NEWID()) ORDER BY lt.TaskGUIDVersion DESC) [RowID]
                    FROM @LegionellaTasksData lt
                ) lt
                LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
				LEFT JOIN LegionellaOutlet lo WITH (NOLOCK) ON lt.LegionellaAssetOutletID = lo.LegionellaOutletID AND lt.RowType = 4

				OUTER APPLY
				(
					SELECT
						ltaskai.LegionellaOutletSentinel [LegionellaOutletSentinel]
					FROM
						LegionellaTask ltask
						INNER JOIN LegionellaAssetOutletTaskAutoInsert ltaskai WITH (NOLOCK) ON ltask.LegionellaTaskAutoInsertID = ltaskai.LegionellaTaskAutoInsertID
					WHERE
						ltask.LegionellaTaskID = lt.LegionellaTaskID
				) los
            WHERE lt.RowID = 1 AND (CASE WHEN los.LegionellaOutletSentinel >= 0 THEN 0 ELSE 1 END) = 1
        END
        ELSE
        BEGIN -- Additional columns included.
            INSERT INTO @LegionellaRiskItemsData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskRating, RiskColour, RiskRatingSortOrder, JobNo, SiteAddress, SitePostcode, BuildingDesignation, AssetOutletSystemRef, Location, RiskDescription, Action, LegionellaFrequencyCategoryID, FrequencyCategory, LegionellaPriorityRatingID, PriorityRating, PriorityColour)
            SELECT
                lt.SiteID,
                lt.JobID,
                lt.RowType,
                lt.LegionellaID,
                lt.LegGUID,
                lt.LegGUIDVersion,
                lt.LegionellaAssetOutletID,
                lt.AssetOutletGUID,
                lt.AssetOutletGUIDVersion,
                lt.LegionellaLocationID,
                lt.LocationGUID,
                lt.LocationGUIDVersion,
                lt.LegionellaTaskID,
                lt.TaskGUID,
                lt.TaskGUIDVersion,
                lrr.LegionellaRiskRatingID,
                ISNULL(lrr.Description, 'None') [RiskRating],
                '#' + ISNULL(lrr.RiskColour, 'CCCCCC') [RiskColour],
                lrr.SortOrder [RiskRatingSortOrder],
                j.JobNo,
                si.Address [SiteAddress],
                si.Postcode [SitePostcode],
                ISNULL(NULLIF(l.BuildingDesignation, ''), 'No Building Designation') [BuildingDesignation],
                ISNULL(NULLIF(ISNULL(la.SystemRef, lo.SystemRef), ''), 'N/A') [AssetOutletSystemRef],
                ISNULL(ISNULL(la.Location, ll.Location), '') [Location],
                ISNULL(lt.RiskDescription, 'N/A') [RiskDescription],
                ISNULL(lt.Action, 'N/A') [Action],
                lfc.LegionellaFrequencyCategoryID,
                ISNULL(lfc.Description, '') [FrequencyCategory],
                lpr.LegionellaPriorityRatingID,
                ISNULL(lpr.ShortDescription, 'None') [PriorityRating],
                '#' + ISNULL(lpr.PriorityColour, 'CCCCCC') [PriorityColour]
            FROM
                (
                    SELECT -- Get each Legionella Task record with the max GUID.
                        lt.SiteID,
                        lt.JobID,
                        lt.RowType,
                        lt.LegionellaID,
                        lt.LegGUID,
                        lt.LegGUIDVersion,
                        lt.LegionellaAssetOutletID,
                        lt.AssetOutletGUID,
                        lt.AssetOutletGUIDVersion,
                        lt.LegionellaLocationID,
                        lt.LocationGUID,
                        lt.LocationGUIDVersion,
                        lt.LegionellaTaskID,
                        lt.TaskGUID,
                        lt.TaskGUIDVersion,
                        lt.LegionellaRiskRatingID,
                        lt.RiskDescription,
                        lt.Action,
                        lt.LegionellaFrequencyCategoryID,
                        lt.LegionellaPriorityRatingID,
                        ROW_NUMBER() OVER (PARTITION BY ISNULL(lt.TaskGUID, NEWID()) ORDER BY lt.TaskGUIDVersion DESC) [RowID]
                    FROM @LegionellaTasksData lt
                ) lt
                INNER JOIN Job j WITH (NOLOCK) ON lt.JobID = j.JobID
                INNER JOIN Site si WITH (NOLOCK) ON lt.SiteID = si.SiteID
                INNER JOIN Legionella l WITH (NOLOCK) ON lt.LegionellaID = l.LegionellaID
                LEFT JOIN LegionellaAsset la WITH (NOLOCK) ON lt.LegionellaAssetOutletID = la.LegionellaAssetID AND lt.RowType = 2
                LEFT JOIN LegionellaLocation ll WITH (NOLOCK) ON lt.LegionellaLocationID = ll.LegionellaLocationID
                LEFT JOIN LegionellaOutlet lo WITH (NOLOCK) ON lt.LegionellaAssetOutletID = lo.LegionellaOutletID AND lt.RowType = 4
                LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
                LEFT JOIN LegionellaFrequencyCategory lfc WITH (NOLOCK) ON lt.LegionellaFrequencyCategoryID = lfc.LegionellaFrequencyCategoryID
                LEFT JOIN LegionellaPriorityRating lpr WITH (NOLOCK) ON lt.LegionellaPriorityRatingID = lpr.LegionellaPriorityRatingID

				OUTER APPLY
				(
					SELECT
						ltaskai.LegionellaOutletSentinel [LegionellaOutletSentinel]
					FROM
						LegionellaTask ltask
						INNER JOIN LegionellaAssetOutletTaskAutoInsert ltaskai WITH (NOLOCK) ON ltask.LegionellaTaskAutoInsertID = ltaskai.LegionellaTaskAutoInsertID
					WHERE
						ltask.LegionellaTaskID = lt.LegionellaTaskID
				) los
            WHERE lt.RowID = 1 AND (CASE WHEN los.LegionellaOutletSentinel >= 0 THEN 0 ELSE 1 END) = 1
        END
    END

    -- Get the default Outlet Temperature Limits for the Company.
    DECLARE @ColdMin FLOAT = 0, @ColdMax FLOAT = 0, @HotMin FLOAT = 0, @HotMax FLOAT = 0, @MixedMin FLOAT = 0, @MixedMax FLOAT = 0, @MainsMin FLOAT = 0, @MainsMax FLOAT = 0
    IF @OutletsOutOfSpec = 1
    BEGIN
        SELECT TOP 1
            @ColdMin = ISNULL(ColdMin, 0),
            @ColdMax = ISNULL(ColdMax, 0),
            @HotMin = ISNULL(HotMin, 0),
            @HotMax = ISNULL(HotMax, 0),
            @MixedMin = ISNULL(MixedMin, 0),
            @MixedMax = ISNULL(MixedMax, 0),
            @MainsMin = ISNULL(MainsMin, 0),
            @MainsMax = ISNULL(MainsMax, 0)
        FROM SiteLegionellaTemperatureLimits WITH (NOLOCK)
        WHERE SiteID = -1 AND Deleted IS NULL
    END

    -- Get the Legionella Outlets out of Spec for each Site.
    DECLARE @LegionellaOutletsOutOfSpec TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegionellaLocationID INT NOT NULL, LegionellaOutletID INT NOT NULL, Cold DECIMAL(10, 2), Hot DECIMAL(10, 2), Mixed DECIMAL(10, 2), Mains DECIMAL(10, 2), ColdMin FLOAT, ColdMax FLOAT, HotMin FLOAT, HotMax FLOAT, MixedMin FLOAT, MixedMax FLOAT, MainsMin FLOAT, MainsMax FLOAT, TempOutOfSpec BIT NOT NULL, /* ADDITIONAL COLUMNS */ JobNo INT, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), BuildingDesignation VARCHAR(50), AssetOutletSystemRef VARCHAR(8000), Location VARCHAR(8000))

    IF @OutletsOutOfSpec = 1
    BEGIN
        IF @ReturnAsChart = 1 -- Basic columns only.
        BEGIN
            INSERT INTO @LegionellaOutletsOutOfSpec (SiteID, JobID, LegionellaID, LegionellaLocationID, LegionellaOutletID, Cold, Hot, Mixed, Mains, ColdMin, ColdMax, HotMin, HotMax, MixedMin, MixedMax, MainsMin, MainsMax, TempOutOfSpec)
            SELECT
                lo.SiteID,
                lo.JobID,
                lo.LegionellaID,
                lo.LegionellaLocationID,
                lo.LegionellaOutletID,
                lo.Cold,
                lo.Hot,
                lo.Mixed,
                lo.Mains,
                ISNULL(siltl.ColdMin, @ColdMin) [ColdMin],
                ISNULL(siltl.ColdMax, @ColdMax) [ColdMax],
                ISNULL(siltl.HotMin, @HotMin) [HotMin],
                ISNULL(siltl.HotMax, @HotMax) [HotMax],
                ISNULL(siltl.MixedMin, @MixedMin) [MixedMin],
                ISNULL(siltl.MixedMax, @MixedMax) [MixedMax],
                ISNULL(siltl.MainsMin, @MainsMin) [MainsMin],
                ISNULL(siltl.MainsMax, @MainsMax) [MainsMax],
                CASE
                    WHEN lo.Cold < ISNULL(siltl.ColdMin, @ColdMin) OR lo.Cold > ISNULL(siltl.ColdMax, @ColdMax) THEN 1
                    WHEN lo.Hot < ISNULL(siltl.HotMin, @HotMin) OR lo.Hot > ISNULL(siltl.HotMax, @HotMax) THEN 1
                    WHEN lo.Mixed < ISNULL(siltl.MixedMin, @MixedMin) OR lo.Mixed > ISNULL(siltl.MixedMax, @MixedMax) THEN 1
                    WHEN lo.Mains < ISNULL(siltl.MainsMin, @MainsMin) OR lo.Mains > ISNULL(siltl.MainsMax, @MainsMax) THEN 1
                    ELSE 0
                END [TempOutOfSpec]
            FROM
                @LegionellaOutletGUIDData lo
                LEFT JOIN SiteLegionellaTemperatureLimits siltl WITH (NOLOCK) ON lo.SiteID = siltl.SiteID AND siltl.Deleted IS NULL
        END
        ELSE
        BEGIN -- Additional columns included.
            INSERT INTO @LegionellaOutletsOutOfSpec (SiteID, JobID, LegionellaID, LegionellaLocationID, LegionellaOutletID, Cold, Hot, Mixed, Mains, ColdMin, ColdMax, HotMin, HotMax, MixedMin, MixedMax, MainsMin, MainsMax, TempOutOfSpec, JobNo, SiteAddress, SitePostcode, BuildingDesignation, AssetOutletSystemRef, Location)
            SELECT
                a.SiteID,
                a.JobID,
                a.LegionellaID,
                a.LegionellaLocationID,
                a.LegionellaOutletID,
                a.Cold,
                a.Hot,
                a.Mixed,
                a.Mains,
                a.ColdMin,
                a.ColdMax,
                a.HotMin,
                a.HotMax,
                a.MixedMin,
                a.MixedMax,
                a.MainsMin,
                a.MainsMax,
                a.TempOutOfSpec,
                a.JobNo,
                a.SiteAddress,
                a.SitePostcode,
                a.BuildingDesignation,
                a.AssetOutletSystemRef,
                a.Location
            FROM
            (
                SELECT
                    lo.SiteID,
                    lo.JobID,
                    lo.LegionellaID,
                    lo.LegionellaLocationID,
                    lo.LegionellaOutletID,
                    lo.Cold,
                    lo.Hot,
                    lo.Mixed,
                    lo.Mains,
                    ISNULL(siltl.ColdMin, @ColdMin) [ColdMin],
                    ISNULL(siltl.ColdMax, @ColdMax) [ColdMax],
                    ISNULL(siltl.HotMin, @HotMin) [HotMin],
                    ISNULL(siltl.HotMax, @HotMax) [HotMax],
                    ISNULL(siltl.MixedMin, @MixedMin) [MixedMin],
                    ISNULL(siltl.MixedMax, @MixedMax) [MixedMax],
                    ISNULL(siltl.MainsMin, @MainsMin) [MainsMin],
                    ISNULL(siltl.MainsMax, @MainsMax) [MainsMax],
                    CASE
                        WHEN lo.Cold < ISNULL(siltl.ColdMin, @ColdMin) OR lo.Cold > ISNULL(siltl.ColdMax, @ColdMax) THEN 1
                        WHEN lo.Hot < ISNULL(siltl.HotMin, @HotMin) OR lo.Hot > ISNULL(siltl.HotMax, @HotMax) THEN 1
                        WHEN lo.Mixed < ISNULL(siltl.MixedMin, @MixedMin) OR lo.Mixed > ISNULL(siltl.MixedMax, @MixedMax) THEN 1
                        WHEN lo.Mains < ISNULL(siltl.MainsMin, @MainsMin) OR lo.Mains > ISNULL(siltl.MainsMax, @MainsMax) THEN 1
                        ELSE 0
                    END [TempOutOfSpec],
                    j.JobNo,
                    si.Address [SiteAddress],
                    si.Postcode [SitePostcode],
                    ISNULL(NULLIF(l.BuildingDesignation, ''), 'No Building Designation') [BuildingDesignation],
                    ISNULL(NULLIF(loj.SystemRef, ''), 'N/A') [AssetOutletSystemRef],
                    ISNULL(ll.Location, '') [Location]
                FROM
                    @LegionellaOutletGUIDData lo
                    INNER JOIN Job j WITH (NOLOCK) ON lo.JobID = j.JobID
                    INNER JOIN Site si WITH (NOLOCK) ON lo.SiteID = si.SiteID
                    LEFT JOIN SiteLegionellaTemperatureLimits siltl WITH (NOLOCK) ON si.SiteID = siltl.SiteID AND siltl.Deleted IS NULL
                    INNER JOIN Legionella l WITH (NOLOCK) ON lo.LegionellaID = l.LegionellaID
                    INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON lo.LegionellaLocationID = ll.LegionellaLocationID
                    INNER JOIN LegionellaOutlet loj WITH (NOLOCK) ON lo.LegionellaOutletID = loj.LegionellaOutletID
            ) a
            WHERE
                CASE WHEN @ReturnAsChart = 0 AND @OutletsOutOfSpec = 1 -- If returning for the grid, only get Outlets Within/Out of Spec.
                    THEN
                        CASE WHEN a.TempOutOfSpec = @OutOfSpec
                            THEN 1
                            ELSE 0
                        END
                    ELSE 1
                END = 1
        END
    END
	
	DECLARE @LegionellaTaskPrioritiesData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, RowType INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, LegionellaAssetOutletID INT, AssetOutletGUID VARCHAR(MAX), AssetOutletGUIDVersion INT, LegionellaLocationID INT, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaTaskID INT NOT NULL, TaskGUID VARCHAR(MAX), TaskGUIDVersion INT, LegionellaRiskRatingID INT, RiskRating VARCHAR(MAX), RiskColour VARCHAR(15), RiskRatingSortOrder INT, /* ADDITIONAL COLUMNS */ JobNo INT, SiteAddress VARCHAR(200), SitePostcode VARCHAR(10), BuildingDesignation VARCHAR(50), AssetOutletSystemRef VARCHAR(8000), Location VARCHAR(8000), RiskDescription VARCHAR(MAX), Action VARCHAR(MAX), LegionellaFrequencyCategoryID INT, FrequencyCategory VARCHAR(8000), LegionellaPriorityRatingID INT, PriorityRating VARCHAR(20), PriorityColour VARCHAR(15))

	IF @TaskPriorities = 1
	BEGIN
		INSERT INTO @LegionellaTaskPrioritiesData (SiteID, JobID, RowType, LegionellaID, LegGUID, LegGUIDVersion, LegionellaAssetOutletID, AssetOutletGUID, AssetOutletGUIDVersion, LegionellaLocationID, LocationGUID, LocationGUIDVersion, LegionellaTaskID, TaskGUID, TaskGUIDVersion, LegionellaRiskRatingID, RiskRating, RiskColour, RiskRatingSortOrder, JobNo, SiteAddress, SitePostcode, BuildingDesignation, AssetOutletSystemRef, Location, RiskDescription, Action, LegionellaFrequencyCategoryID, FrequencyCategory, LegionellaPriorityRatingID, PriorityRating, PriorityColour)
		SELECT
			lt.SiteID,
			lt.JobID,
			lt.RowType,
			lt.LegionellaID,
			lt.LegGUID,
			lt.LegGUIDVersion,
			lt.LegionellaAssetOutletID,
			lt.AssetOutletGUID,
			lt.AssetOutletGUIDVersion,
			lt.LegionellaLocationID,
			lt.LocationGUID,
			lt.LocationGUIDVersion,
			lt.LegionellaTaskID,
			lt.TaskGUID,
			lt.TaskGUIDVersion,
			lrr.LegionellaRiskRatingID,
			ISNULL(lrr.Description, 'None') [RiskRating],
			'#' + ISNULL(lrr.RiskColour, 'CCCCCC') [RiskColour],
			lrr.SortOrder [RiskRatingSortOrder],
			j.JobNo,
			si.Address [SiteAddress],
			si.Postcode [SitePostcode],
			ISNULL(NULLIF(l.BuildingDesignation, ''), 'No Building Designation') [BuildingDesignation],
			ISNULL(NULLIF(ISNULL(la.SystemRef, lo.SystemRef), ''), 'N/A') [AssetOutletSystemRef],
			ISNULL(ISNULL(la.Location, ll.Location), '') [Location],
			ISNULL(lt.RiskDescription, 'N/A') [RiskDescription],
			ISNULL(lt.Action, 'N/A') [Action],
			lfc.LegionellaFrequencyCategoryID,
			ISNULL(lfc.Description, '') [FrequencyCategory],
			lpr.LegionellaPriorityRatingID,
			ISNULL(lpr.ShortDescription, 'None') [PriorityRating],
			'#' + ISNULL(lpr.PriorityColour, 'CCCCCC') [PriorityColour]
		FROM
			(
				SELECT -- Get each Legionella Task record with the max GUID.
					lt.SiteID,
					lt.JobID,
					lt.RowType,
					lt.LegionellaID,
					lt.LegGUID,
					lt.LegGUIDVersion,
					lt.LegionellaAssetOutletID,
					lt.AssetOutletGUID,
					lt.AssetOutletGUIDVersion,
					lt.LegionellaLocationID,
					lt.LocationGUID,
					lt.LocationGUIDVersion,
					lt.LegionellaTaskID,
					lt.TaskGUID,
					lt.TaskGUIDVersion,
					lt.LegionellaRiskRatingID,
					lt.RiskDescription,
					lt.Action,
					lt.LegionellaFrequencyCategoryID,
					lt.LegionellaPriorityRatingID,
					ROW_NUMBER() OVER (PARTITION BY ISNULL(lt.TaskGUID, NEWID()) ORDER BY lt.TaskGUIDVersion DESC) [RowID]
				FROM @LegionellaTasksData lt
			) lt
			INNER JOIN Job j WITH (NOLOCK) ON lt.JobID = j.JobID
			INNER JOIN Site si WITH (NOLOCK) ON lt.SiteID = si.SiteID
			INNER JOIN Legionella l WITH (NOLOCK) ON lt.LegionellaID = l.LegionellaID
			LEFT JOIN LegionellaAsset la WITH (NOLOCK) ON lt.LegionellaAssetOutletID = la.LegionellaAssetID AND lt.RowType = 2
			LEFT JOIN LegionellaLocation ll WITH (NOLOCK) ON lt.LegionellaLocationID = ll.LegionellaLocationID
			LEFT JOIN LegionellaOutlet lo WITH (NOLOCK) ON lt.LegionellaAssetOutletID = lo.LegionellaOutletID AND lt.RowType = 4
			LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
			LEFT JOIN LegionellaFrequencyCategory lfc WITH (NOLOCK) ON lt.LegionellaFrequencyCategoryID = lfc.LegionellaFrequencyCategoryID
			LEFT JOIN LegionellaPriorityRating lpr WITH (NOLOCK) ON lt.LegionellaPriorityRatingID = lpr.LegionellaPriorityRatingID

			OUTER APPLY
			(
				SELECT
					ltaskai.LegionellaOutletSentinel [LegionellaOutletSentinel]
				FROM
					LegionellaTask ltask
					INNER JOIN LegionellaAssetOutletTaskAutoInsert ltaskai WITH (NOLOCK) ON ltask.LegionellaTaskAutoInsertID = ltaskai.LegionellaTaskAutoInsertID
				WHERE
					ltask.LegionellaTaskID = lt.LegionellaTaskID
			) los

		WHERE lt.RowID = 1 AND (CASE WHEN los.LegionellaOutletSentinel >= 0 THEN 0 ELSE 1 END) = 1

		--Task Frequencies data
		DECLARE @LegionellaTaskPriorities TABLE (IndexID int identity(1,1), Category varchar(max), PriorityRatingID int, Count int, Colour varchar(max), SortOrder int)

		INSERT INTO @LegionellaTaskPriorities

		SELECT
			lpr.Description,
			lpr.LegionellaPriorityRatingID,
			PriorityRatingCategory.Count,
			'#' + lpr.PriorityColour,
			lpr.SortOrder
		FROM
			LegionellaPriorityRating lpr
		
			OUTER APPLY
			(
				SELECT
					COUNT(*) [Count]
				FROM
					@LegionellaTaskPrioritiesData ltpd
				WHERE
					ltpd.LegionellaPriorityRatingID = lpr.LegionellaPriorityRatingID
			) PriorityRatingCategory
	END

    -- Add more to the Total Compliance data for Sites without a Survey.
    IF @TotalCompliance = 1
    BEGIN
        INSERT INTO #TotalComplianceData (SiteID, Post2000, UnmanagedSiteLeg)
        SELECT
            si.SiteID,
            si.Post2000,
            si.UnmanagedSiteLeg
        FROM
            @ClientSiteData csd
            INNER JOIN Site si WITH (NOLOCK) ON csd.SiteID = si.SiteID
        WHERE
            si.SiteID NOT IN (SELECT SiteID FROM #TotalComplianceData)
				AND
			si.UnmanagedSiteLeg = 0
        GROUP BY
            si.SiteID,
            si.Post2000,
            si.UnmanagedSiteLeg
    END

    -- Get the total number of items.
    DECLARE @TotalComplianceItems INT = (SELECT COUNT(*) FROM #TotalComplianceData)
    DECLARE @TotalRiskItems INT = (SELECT COUNT(*) FROM @LegionellaRiskItemsData)
    DECLARE @TotalLowUseOutlets INT = (SELECT COUNT(*) FROM @LegionellaOutletGUIDData WHERE LowUse = 1)
    DECLARE @TotalOutletsOutOfSpec INT = (SELECT COUNT(*) FROM @LegionellaOutletsOutOfSpec)

    -- Start the main SELECT.
    IF @ReturnAsChart = 1
    BEGIN
        SELECT
            CASE
                --WHEN Post2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSiteLeg = 1 THEN 'Un-managed Site (No LRA required)'
                WHEN JobID IS NOT NULL THEN 'Assessed (LRA)'
                ELSE 'Not Assessed (LRA)'
            END [category],
            CASE
                --WHEN Post2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSiteLeg = 1 THEN '#15527F'
                WHEN JobID IS NOT NULL THEN '#AFD8F8'
                ELSE '#F6BD0F'
            END [Colour],
            COUNT(*) [Share],
            @TotalComplianceItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            CAST(
                CASE WHEN JobID IS NOT NULL
                    THEN 1
                    ELSE 0
                END AS BIT) [Surveyed]
        FROM #TotalComplianceData
        WHERE @TotalCompliance = 1
        GROUP BY
             CASE
                --WHEN Post2000 = 1 THEN 'Post 2000'
                WHEN UnmanagedSiteLeg = 1 THEN 'Un-managed Site (No LRA required)'
                WHEN JobID IS NOT NULL THEN 'Assessed (LRA)'
                ELSE 'Not Assessed (LRA)'
            END,
            CASE
                --WHEN Post2000 = 1 THEN '#CCCCCC'
                WHEN UnmanagedSiteLeg = 1 THEN '#15527F'
                WHEN JobID IS NOT NULL THEN '#AFD8F8'
                ELSE '#F6BD0F'
            END,
            CASE WHEN JobID IS NOT NULL
                THEN 1
                ELSE 0
            END
        ORDER BY
            Surveyed,
            category

        SELECT
            lrr.RiskRating [category],
            '#' + ISNULL(lrr.RiskColour, 'CCCCCC') [Colour],
            COUNT(lrid.LegionellaTaskID) [Share],
            @TotalRiskItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            lrr.RiskRatingSortOrder [SortOrder],
            lrr.LegionellaRiskRatingID
        FROM
            (
                SELECT LegionellaRiskRatingID, Description [RiskRating], RiskColour, SortOrder [RiskRatingSortOrder] FROM LegionellaRiskRating WITH (NOLOCK) WHERE Deleted IS NULL
            ) lrr
            LEFT JOIN @LegionellaRiskItemsData lrid ON lrr.LegionellaRiskRatingID = ISNULL(lrid.LegionellaRiskRatingID, -1)
        WHERE @RiskItems = 1
        GROUP BY
            lrr.RiskRating,
            '#' + ISNULL(lrr.RiskColour, 'CCCCCC'),
            lrr.RiskRatingSortOrder,
            lrr.LegionellaRiskRatingID
        ORDER BY
            SortOrder,
            category

        SELECT
            CASE WHEN Flushed = 1
                THEN 'Flushed'
                ELSE 'Not Flushed'
            END [category],
            CASE WHEN Flushed = 1
                THEN '#AFD8F8'
                ELSE '#CCCCCC'
            END [Colour],
            COUNT(*) [Share],
            @TotalLowUseOutlets [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            Flushed
        FROM @LegionellaOutletGUIDData
        WHERE @LowUseOutlets = 1 AND LowUse = 1
        GROUP BY
            CASE WHEN Flushed = 1
                THEN 'Flushed'
                ELSE 'Not Flushed'
            END,
            CASE WHEN Flushed = 1
                THEN '#AFD8F8'
                ELSE '#CCCCCC'
            END,
            Flushed
        ORDER BY
            Flushed DESC,
            category

        SELECT
            CASE WHEN TempOutOfSpec = 1
                THEN 'Outside of Specification'
                ELSE 'Within Specification'
            END [category],
            CASE WHEN TempOutOfSpec = 1
                THEN '#E8412D'
                ELSE '#00B050'
            END [Colour],
            COUNT(*) [Share],
            @TotalOutletsOutOfSpec [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            TempOutOfSpec
        FROM @LegionellaOutletsOutOfSpec
        WHERE @OutletsOutOfSpec = 1
        GROUP BY
            CASE WHEN TempOutOfSpec = 1
                THEN 'Outside of Specification'
                ELSE 'Within Specification'
            END,
            CASE WHEN TempOutOfSpec = 1
                THEN '#E8412D'
                ELSE '#00B050'
            END,
            TempOutOfSpec
        ORDER BY
            TempOutOfSpec,
            category

		SELECT
			ltp.Category [category],
			ltp.Colour [Colour],
			ltp.Count [Share],
			Total.Count [TotalItems],
			CAST(1 AS BIT) [VisibleInLegend],
			ltp.SortOrder [SortOrder],
			ltp.PriorityRatingID [LegionellaPriorityRatingID]
		FROM
			@LegionellaTaskPriorities ltp

			OUTER APPLY
			(
				SELECT
					SUM(ltp2.Count) [Count]
				FROM
					@LegionellaTaskPriorities ltp2
			) Total
		WHERE
			@TaskPriorities = 1
    END
    ELSE
    BEGIN
        SELECT * FROM #TotalComplianceData
        WHERE @TotalCompliance = 1
        ORDER BY SiteID

        SELECT * FROM @LegionellaRiskItemsData
        WHERE @RiskItems = 1
        ORDER BY SiteID

        SELECT * FROM @LegionellaOutletGUIDData
        WHERE @LowUseOutlets = 1 AND LowUse = 1
        ORDER BY SiteID

        SELECT * FROM @LegionellaOutletsOutOfSpec
        WHERE @OutletsOutOfSpec = 1
        ORDER BY SiteID
		
        SELECT * FROM @LegionellaTaskPrioritiesData
        WHERE @TaskPriorities = 1
        ORDER BY SiteID
    END

    -- Clear up temp tables.
    DROP TABLE #TotalComplianceData


    SET NOCOUNT OFF;
END

GO

ALTER FUNCTION [dbo].[DetailedSampleInformation] (@SampleID INT, @ShowReinspectionOverride INT = 0)
RETURNS TABLE
AS
RETURN
(
    SELECT
        t.ElementTypeId,
        t.ElementName, 
        CASE
            -- Level of Identification.
            WHEN t.ElementTypeID = 5 THEN
                CASE WHEN s.AsSample = 1 AND scd.SampleResult = 0
                    THEN '(0) No Asbestos Detected as ' + s.SampleRef
                    ELSE COALESCE(eimso.Description, eime.Description, eim.ShortDescription, eim.Description)
                END

            -- Asbestos Type.
            When t.ElementTypeId = 8 Then 
                CASE WHEN scd.Removed = 1
                    Then '(0) No Asbestos Detected'
                    Else scd.AsbestosType
                End

            -- Product Description.
            When t.ElementTypeId =  6 Then scd.Classification

            -- Rec. Action.
            When t.ElementTypeId = 20 Then
                Case When Lower(CASE WHEN scd.Removed = 1 Then '(0) No Asbestos Detected' Else scd.AsbestosType End) = '(0) no asbestos detected'
                    Then 'No further action required'
                    Else scd.RecommendedAction
                End

            -- Surface Treatment.
            When t.ElementTypeId = 10 And Lower(scd.AsbestosType) = '(0) no asbestos detected' Then
                'N/A'
            Else -- Anything else.
                COALESCE(e.ElementText, eimso.Description, eime.Description, eim.ShortDescription, eim.Description)
        End [Value],
        t.SortOrder
    From
        ElementType t WITH (NOLOCK)
        Left Outer Join Element e WITH (NOLOCK) On e.ElementTypeId = t.ElementTypeId And e.SampleId = @SampleID
        Left Outer Join ElementIntMeaning eim WITH (NOLOCK) On e.ElementIntMeaningId = eim.ElementIntMeaningId
        Left Outer Join ElementIntMeaningExtended eime WITH (NOLOCK) On e.ElementIntMeaningExtendedId = eime.ElementIntMeaningExtendedId
        Left Outer Join ElementIntMeaningSubOption eimso WITH (NOLOCK) On e.ElementIntMeaningSubOptionId = eimso.ElementIntMeaningSubOptionId
        Left Outer Join SampleComputedData scd WITH (NOLOCK) On scd.SampleId = @SampleID
        Outer Apply
        (
            SELECT
                s.SampleRef,
                s.AsSample,
                Cast(PatIndex('%reinspection%', sut.Description) as bit) as IsReinspection
            From
                Sample s WITH (NOLOCK)
                Inner Join Room rm WITH (NOLOCK) On s.RoomId = rm.RoomId
                Inner Join Floorplan f WITH (NOLOCK) On rm.FloorplanId = f.FloorplanId
                Inner Join Register r WITH (NOLOCK) On f.RegisterId = r.RegisterId
                Inner Join Survey su WITH (NOLOCK) On r.SurveyId = su.SurveyId
                Inner Join SurveyType sut WITH (NOLOCK) On su.SurveyTypeId = sut.SurveyTypeId
            Where
                s.SampleID = @SampleID
        ) s
    Where
        (
              (
                    (Not scd.SampleResult = 0  AND scd.Removed = 0)
                          Or
                    (NOT scd.SampleResult = 0 AND scd.Removed = 1 And t.ElementTypeId In (5,6,8))
              )
                    Or
              (scd.SampleResult = 0 And t.ElementTypeId In (5,6,8)) -- Level of Identification, Product Description & Asbestos Type
        )
              AND
        (
        (
            @ShowReinspectionOverride=0
            AND 
              (
                Not t.ElementTypeId = 48 -- Reinspection State
                          OR
                  (t.ElementTypeId = 48 And s.IsReinspection = 1)
            )
        )
        OR
        (
            @ShowReinspectionOverride=1
        )
        
        )
)
GO
ALTER PROCEDURE [dbo].[GetSampleDetailedInformation]
    @PortalUserID INT,
    @SampleID INT
/**********************************************************************
** Overview: Get all details needed for managing a Sample via the Portal.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set variables for logic.
    DECLARE @CanEditPA BIT, @CanCreatePA BIT, @Management BIT, @PriorityAssessment BIT, @ClientID INT, @SuperUser BIT
    SELECT
        @CanEditPA = CASE WHEN pu.PortalAdministrator = 1 THEN 1 ELSE 0 END,
        @CanCreatePA = CASE WHEN pu.PortalAdministrator = 1 THEN 1 ELSE 0 END,
        @Management = r.Management,
        @PriorityAssessment = r.PriorityAssessment,
        @ClientID = j.ClientID,
		@SuperUser = pu.SuperUser
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID
        INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
        INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        CROSS JOIN PortalUser pu WITH (NOLOCK)
    WHERE
        s.SampleID = @SampleID
            AND
        pu.PortalUserID = @PortalUserID

    -- 1st RS: Get basic details for the User/Sample.
    SELECT
        @CanEditPA [CanEditPA],
        @CanCreatePA [CanCreatePA],
        s.PhotoID,
        scd.Quantity,
        scd.SiteID
    FROM
        Sample s WITH (NOLOCK)
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON s.SampleID = scd.SampleID
    WHERE
        s.SampleID = @SampleID

    -- Get the ElementTypeIDs we need for this Sample.
    DECLARE @ElementTypeData TABLE (ElementTypeID INT, Editable BIT)

    -- Get the ElementTypeIDs configured for mobileTEAMS (MobileConfigTypeID = 8,9).
    INSERT INTO @ElementTypeData (ElementTypeID, Editable)
    SELECT et.ElementTypeID, 0
    FROM 
        ElementType et WITH (NOLOCK)
        INNER JOIN MobileConfig mc WITH (NOLOCK) ON et.ElementTypeID = mc.MobileConfigInt AND mc.MobileConfigTypeID IN (8, 9) AND mc.Deleted IS NULL  -- MA, PA

    -- Get the ElementTypeIDs configured for the Position Description/Comments system (MobileConfigTypeID = 16).
    IF EXISTS (SELECT 1 FROM MobileConfig WITH (NOLOCK) WHERE MobileConfigTypeID = 16 AND MobileConfigInt = 1 AND Deleted IS NULL)
    BEGIN
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (1, 0)
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (31, 0)
    END

    -- Get the ElementTypeIDs configured for the required Quantity system (MobileConfigTypeID = 41).
    IF EXISTS (SELECT 1 FROM MobileConfig WITH (NOLOCK) WHERE MobileConfigTypeID = 41 AND MobileConfigInt = 1 AND Deleted IS NULL)
    BEGIN
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (3, 0)
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (28, 0)
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (29, 0)
    END

    -- Get the ElementTypeIDs configured for the standard Management types.
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (3, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (4, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (5, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (6, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (7, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (8, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (9, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (10, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (13, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (20, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (26, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (22, 0) -- Assume Register.Management = 1 here
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (23, 0) -- Assume Register.Management = 1 here
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (24, 0) -- Assume Register.Management = 1 here
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (27, 0) -- Assume Register.Management = 1 here

    -- Get the ElementTypeIDs configured for the SampleDateOfNextReview (MobileConfigTypeID = 64)
    IF EXISTS (SELECT 1 FROM MobileConfig WITH (NOLOCK) WHERE MobileConfigTypeID = 64 AND MobileConfigInt = 1 AND Deleted IS NULL)
    BEGIN
        INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (27, 0)
    END

    -- Get the ElementTypeIDs configured for the Review comments and Reinspection state.
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (45, 0)
    INSERT INTO @ElementTypeData (ElementTypeID, Editable) VALUES (48, 0)

    -- Remove ElementTypeIDs that don't apply to the Portal.
    DELETE FROM @ElementTypeData
    WHERE ElementTypeID IN (1, 3, 4, 22, 26, 27)

    -- Mark the ElementTypeIDs which are editable.
    UPDATE @ElementTypeData
    SET Editable = 1
    WHERE ElementTypeID IN (11, 12, 13, 14, 15, 16, 17, 18, 19, 31, 45, 48)

	IF @SuperUser = 1
	BEGIN
	UPDATE @ElementTypeData
	SET Editable = 1
	WHERE ElementTypeID = 20
	END

    -- 2nd RS: Get the ElementTypes we need to display on the Portal with the current value selected.
    SELECT DISTINCT
        dsi.ElementName,
        dsi.ElementTypeID,
        dsi.Value,
        et.Editable,
        dsi.SortOrder
    FROM
        DetailedSampleInformation(@SampleID, 1) dsi
        INNER JOIN @ElementTypeData et ON dsi.ElementTypeID = et.ElementTypeID
    ORDER BY
        dsi.SortOrder

    -- 3rd RS: Get the ElementIntMeaning values available for each ElementType.
    SELECT DISTINCT
        et.ElementTypeID,
        ISNULL(eim.Description, eim.ShortDescription) + ISNULL(' | ' + eime.Description, '') + ISNULL(' | ' + eimso.Description, '') [UserText],
        eim.ElementIntValue,
        eim.ElementIntMeaningID,
        eime.ElementIntMeaningExtendedID,
        eimso.ElementIntMeaningSubOptionID,
        CAST(CASE WHEN e.SampleID IS NULL THEN 0 ELSE 1 END AS BIT) [CurrentlySet]
    FROM
        @ElementTypeData et
        INNER JOIN ElementIntMeaning eim WITH (NOLOCK) ON et.ElementTypeID = eim.ElementTypeID
        LEFT JOIN (
            SELECT
                _eime.ElementIntMeaningID,
                _eime.ElementIntMeaningExtendedID,
                _eime.Description
            FROM
                ElementIntMeaningExtended _eime WITH (NOLOCK)
                INNER JOIN ClientSpecificElementIntMeaningExtended _cseime WITH (NOLOCK) ON _eime.ElementIntMeaningExtendedID = _cseime.ElementIntMeaningExtendedID
            WHERE
                _cseime.ClientID = @ClientID
        ) eime ON eim.ElementIntMeaningID = eime.ElementIntMeaningID
        LEFT JOIN ElementIntMeaningSubOption eimso WITH (NOLOCK) ON eim.ElementIntMeaningID = eimso.ElementIntMeaningID
        LEFT JOIN Element e WITH (NOLOCK) ON @SampleID = e.SampleID AND (
            (eim.ElementIntMeaningID = e.ElementIntMeaningID OR eim.ElementIntMeaningID IS NULL)
                AND
            (eime.ElementIntMeaningExtendedID = e.ElementIntMeaningExtendedID OR eime.ElementIntMeaningExtendedID IS NULL)
                AND
            (eimso.ElementIntMeaningSubOptionID = e.ElementIntMeaningSubOptionID OR eimso.ElementIntMeaningSubOptionID IS NULL)
        ) AND et.ElementTypeID = e.ElementTypeID
    WHERE
        et.Editable = 1
    ORDER BY
        et.ElementTypeID,
        eim.ElementIntValue,
        eim.ElementIntMeaningID


    SET NOCOUNT OFF;
END

GO
ALTER PROCEDURE [dbo].[GetAirTests]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @JobNo INT = 0,
    @JobNoSearch INT = 0,
    @GetSiteDocuments INT = 0, /* 0 or NULL = Any, 1 = Yes, 2 = No */
	@AirTestTypeID INT = 0
/**********************************************************************
** Overview: Get a filtered collection of air tests.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @JobNo = NULLIF(@JobNo, 0),
        @JobNoSearch = NULLIF(@JobNoSearch, 0),
        @GetSiteDocuments = ISNULL(@GetSiteDocuments, 0),	
		@AirTestTypeID = NULLIF(@AirTestTypeID, 0)	

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocClientOrderNo VARCHAR(50) = @ClientOrderNo,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocJobNo INT = @JobNo,
        @LocJobNoSearch INT = @JobNoSearch,
        @LocGetSiteDocuments INT = @GetSiteDocuments,
		@LocAirTestType INT = @AirTestTypeID

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @b__onlyshowApprovedAirTestsOnPortal BIT
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @b__onlyshowApprovedAirTestsOnPortal = cfg.b__onlyshowApprovedAirTestsOnPortal
    FROM
        Config cfg WITH (NOLOCK)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END

    -- Get all Air Test Data up front to reduce the main SELECT table scans.
    DECLARE @AirTestData TABLE (IsSiteDocument BIT NOT NULL, JobID INT, JobNo INT, ClientOrderNo VARCHAR(50), Created DATETIME NOT NULL, LastNoteCreated DATETIME, ClientID INT NOT NULL, Client VARCHAR(100) NOT NULL, BranchName VARCHAR(100), SiteID INT NOT NULL, Address VARCHAR(200) NOT NULL, Postcode VARCHAR(10) NOT NULL, UPRN VARCHAR(50), AirTestID INT, AirTestNo INT, AirTestStart DATETIME, AirTestFinish DATETIME, SiteArrival DATETIME, LocationEnclosure VARCHAR(MAX), Status VARCHAR(100), SystemName VARCHAR(2), Approved DATETIME, AirTestTypeID INT, AirTestType VARCHAR(50), AirTestReport VARCHAR(100), SiteDocumentID INT, SiteDocumentsCount INT NOT NULL)

    -- Get normal Air Tests first.
    IF @LocGetSiteDocuments <> 1
    BEGIN
        INSERT INTO @AirTestData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, ClientID, Client, BranchName, SiteID, Address, Postcode, UPRN, AirTestID, AirTestNo, AirTestStart, AirTestFinish, SiteArrival, LocationEnclosure, Status, SystemName, Approved, AirTestTypeID, AirTestType, AirTestReport, SiteDocumentID, SiteDocumentsCount)
        SELECT
            0 [IsSiteDocument],
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            MAX(n.DateCreated) [LastNoteCreated],
            c.ClientID,
            c.Client,
            c.BranchName,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            at.AirTestID,
            at.AirTestNo,
            at.AirTestStart,
            at.AirTestFinish,
            at.SiteArrival,
            at.LocationEnclosure,
            at.Status,
            at.SystemName,
            at.OfficeApprovedDate [Approved],
            att.AirTestTypeID,
            att.Description [AirTestType],
            pdfFile.FileName [AirTestReport],
            NULL [SiteDocumentID],
            0 [SiteDocumentsCount]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN Client c WITH (NOLOCK) ON j.ClientID = c.ClientID AND c.Deleted IS NULL
            INNER JOIN #ClientIdData cid ON c.ClientID = cid.ClientID
            LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN AirTest at WITH (NOLOCK) ON je.JobEmployeeID = at.JobEmployeeID AND CASE WHEN @b__onlyshowApprovedAirTestsOnPortal = 1 THEN CASE WHEN at.OfficeApprovedDate IS NOT NULL THEN 1 ELSE 0 END ELSE 1 END = 1
            INNER JOIN AirTestType att WITH (NOLOCK) ON at.AirTestTypeID = att.AirTestTypeID
            LEFT JOIN Note n WITH (NOLOCK) ON at.AirTestID = n.ItemID AND n.NoteTypeID = 5 AND n.PortalUserID IS NOT NULL
            OUTER APPLY
            (
                SELECT TOP 1 _pf.FileName [FileName]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName LIKE '%\_' + CAST(at.AirTestID AS VARCHAR) + ' (%' ESCAPE '\'
                ORDER BY
                    _pf.DateCreated DESC
            ) pdfFile
        WHERE
            si.Deleted IS NULL
                AND
            j.Cancelled IS NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocClientOrderNo IS NULL -- Client Order No Filter.
                THEN 1
                ELSE CASE WHEN j.ClientOrderNo = @LocClientOrderNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                THEN 1
                ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                THEN 1
                ELSE
                    CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR j.ClientOrderNo = @LocAddressSearchString
                        THEN 1
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocJobNo IS NULL -- Job Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocJobNoSearch IS NULL -- Job Search Filter.
                THEN 1
                ELSE CASE WHEN CAST(j.JobNo AS VARCHAR(50)) LIKE CAST(@LocJobNoSearch AS VARCHAR(50)) + '%' THEN 1 ELSE 0 END
            END = 1
				AND
			CASE 
				WHEN @AirTestTypeID IS NULL
				THEN 1
			ELSE
				CASE WHEN at.AirTestTypeID = @AirTestTypeID THEN 1 ELSE 0 END
			END = 1
        GROUP BY
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            c.ClientID,
            c.Client,
            c.BranchName,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            at.AirTestID,
            at.AirTestNo,
            at.AirTestStart,
            at.AirTestFinish,
            at.SiteArrival,
            at.LocationEnclosure,
            at.Status,
            at.SystemName,
            at.OfficeApprovedDate,
            att.AirTestTypeID,
            att.Description,
            pdfFile.FileName
        ORDER BY
            ISNULL(at.OfficeApprovedDate, at.AirTestStart) DESC,
            j.JobNo DESC,
            at.AirTestNo DESC
    END

    -- Get the Site Documents.
    IF @LocGetSiteDocuments <> 2
    BEGIN
        INSERT INTO @AirTestData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, ClientID, Client, BranchName, SiteID, Address, Postcode, UPRN, AirTestID, AirTestNo, AirTestStart, AirTestFinish, SiteArrival, LocationEnclosure, Status, SystemName, Approved, AirTestTypeID, AirTestType, AirTestReport, SiteDocumentID, SiteDocumentsCount)
        SELECT
            1 [IsSiteDocument],
            NULL [JobID],
            NULL [JobNo],
            NULL [ClientOrderNo],
            sid.Uploaded [Created],
            NULL [LastNoteCreated],
            MAX(c.ClientID) [ClientID],
            MAX(c.Client) [Client],
            MAX(c.BranchName) [BranchName],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            NULL [AirTestID],
            NULL [AirTestNo],
            sid.WorkDate [AirTestStart],
            sid.WorkDate [AirTestFinish],
            NULL [SiteArrival],
            NULL [LocationEnclosure],
            NULL [Status],
            NULL [SystemName],
            NULL [Approved],
            NULL [AirTestTypeID],
            NULL [AirTestType],
            sid.FileName [AirTestReport],
            sid.SiteDocumentID,
            sidc.SiteDocumentCount [SiteDocumentsCount]
        FROM
            Site si WITH (NOLOCK)
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
            INNER JOIN Client c WITH (NOLOCK) ON cs.ClientID = c.ClientID
            INNER JOIN #ClientIdData cid ON c.ClientID = cid.ClientID
            OUTER APPLY
            (
                SELECT TOP 1
                    sid.SiteDocumentID,
                    sid.SiteID,
                    sid.EmployeeID,
                    sid.PortalUserID,
                    sid.FileName,
                    sid.Uploaded,
                    sidi.SiteDocumentInformationID,
                    sidi.WorkDate
                FROM
                    SiteDocument sid WITH (NOLOCK)
                    INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentID = sidi.SiteDocumentID
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 5
                ORDER BY
                    sid.Uploaded DESC
            ) sid
            OUTER APPLY
            (
                SELECT COUNT(*) [SiteDocumentCount]
                FROM SiteDocument sid WITH (NOLOCK)
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 5
            ) sidc
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            sid.SiteDocumentID IS NOT NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            sid.SiteDocumentID,
            sid.FileName,
            sid.Uploaded,
            sid.WorkDate,
            sidc.SiteDocumentCount
        ORDER BY
            sid.Uploaded DESC
    END
    -- Start the main SELECT
    SELECT *
    FROM @AirTestData
    ORDER BY
        ISNULL(Approved, Created) DESC,
        JobNo DESC,
        AirTestNo DESC,
        IsSiteDocument

    -- Clear up temp tables.
    DROP TABLE #ClientIdData

    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaLogBookTasks]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteID INT,
    @DisplayDate DATETIME = NULL,
    @LegionellaTypeID INT = NULL
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @DisplayDate = ISNULL(@DisplayDate, GETDATE()),
        @LegionellaTypeID = NULLIF(@LegionellaTypeID, 0)

    -- Cast DATETIME to DATE. Add one day to the FinishDate - this will set @FinishDate as the next day but it will be at midnight.
    DECLARE @StartDate DATETIME, @FinishDate DATETIME
    SELECT
        @StartDate = CAST(@DisplayDate AS DATE),
        @FinishDate = DATEADD(d, 1, CAST(@DisplayDate AS DATE))

    -- Use a table variable to get all of the ClientIDs.
    DECLARE @ClientTableIDs TABLE (ClientID INT NOT NULL PRIMARY KEY)
    INSERT INTO @ClientTableIDs (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID] FROM dbo.SplitString(@ClientIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s

    -- Use a table variable to get all of the LegionellaIDs.
    DECLARE @LegionellaIDs TABLE (LegionellaID INT NOT NULL PRIMARY KEY)
    INSERT INTO @LegionellaIDs (LegionellaID)
    SELECT l.LegionellaID
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientTableIDs c ON j.ClientID = c.ClientID
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
    WHERE
        j.SiteID = @SiteID
            AND
        j.Cancelled IS NULL
            AND
        CASE WHEN @ProjectGroupID IS NULL -- Project Group filter.
            THEN 1
            ELSE
                CASE WHEN @ProjectGroupID IS NOT NULL AND p.ProjectGroupID = @ProjectGroupID
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @ProjectID IS NULL -- Project filter.
            THEN 1
            ELSE
                CASE WHEN @ProjectID IS NOT NULL AND p.ProjectID = @ProjectID
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @LegionellaTypeID IS NULL -- Legionella Type filter.
            THEN 1
            ELSE
                CASE WHEN @LegionellaTypeID IS NOT NULL AND al.LegionellaTypeID = @LegionellaTypeID
                    THEN 1
                    ELSE 0
                END
        END = 1
    GROUP BY
        l.LegionellaID

    -- Get the Log Book Task data.
    DECLARE @LogBookTasksData TABLE (IndexID INT IDENTITY(1,1), LegionellaTaskID INT NOT NULL, RowType INT NOT NULL, SystemRef VARCHAR(MAX), Location VARCHAR(MAX), Action VARCHAR(MAX), FrequencyCategory VARCHAR(MAX), FrequencyCategorySortOrder INT NOT NULL, RiskColour VARCHAR(MAX), EventRowNo INT NOT NULL, Recorded DATETIME, Comments VARCHAR(MAX), PerformedBy VARCHAR(MAX))

    INSERT INTO @LogBookTasksData (LegionellaTaskID, RowType, SystemRef, Location, Action, FrequencyCategory, FrequencyCategorySortOrder, RiskColour, EventRowNo, Recorded, Comments, PerformedBy)
    SELECT
        lt.LegionellaTaskID,
        lao.RowType,
        lao.AssetOutletSystemRef [SystemRef],
        lao.AssetOutletLocation [Location],
        lt.Action,
        lfc.Description [FrequencyCategory],
        ISNULL(lfc.SortOrder, -1) [FrequencyCategorySortOrder],
        lrr.RiskColour,
        ROW_NUMBER() OVER (PARTITION BY lt.LegionellaTaskID ORDER BY lte.Recorded DESC) [EventRowNo], -- The latest Event for the current Task will be first.
        lte.Recorded,
        lte.Comments,
        ISNULL(e.FullName, ISNULL(pu.FullName, lte.PerformedByThirdParty)) [PerformedBy]
    FROM
        @LegionellaIDs leg
        INNER JOIN (
            SELECT
                1 [RowType],
                _l.LegionellaID,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                NULL [AssetOutletLocation]
            FROM
                Legionella _l WITH (NOLOCK)

            UNION ALL

            SELECT
                2 [RowType],
                _la.LegionellaID,
                _la.LegionellaAssetID [LegionellaAssetOutletID],
                _la.SystemRef [AssetOutletSystemRef],
                NULL [LegionellaLocationID],
                _la.Location [AssetOutletLocation]
            FROM
                LegionellaAsset _la WITH (NOLOCK)
            WHERE
                _la.Deleted IS NULL

            UNION ALL

            SELECT
                3 [RowType],
                _ll.LegionellaID,
                NULL [LegionellaAssetOutletID],
                NULL [AssetOutletSystemRef],
                _ll.LegionellaLocationID,
                _ll.Location [AssetOutletLocation]
            FROM
                LegionellaLocation _ll WITH (NOLOCK)
            WHERE
                _ll.Deleted IS NULL

            UNION ALL

            SELECT
                4 [RowType],
                _ll.LegionellaID,
                _lo.LegionellaOutletID [LegionellaAssetOutletID],
                _lo.SystemRef [AssetOutletSystemRef],
                _ll.LegionellaLocationID,
                _ll.Location [AssetOutletLocation]
            FROM
                LegionellaLocation _ll WITH (NOLOCK)
                INNER JOIN LegionellaOutlet _lo WITH (NOLOCK) ON _ll.LegionellaLocationID = _lo.LegionellaLocationID
            WHERE
                _ll.Deleted IS NULL
                    AND
                _lo.Deleted IS NULL
        ) lao ON leg.LegionellaID = lao.LegionellaID
        INNER JOIN LegionellaTask lt WITH (NOLOCK) ON
            CASE lao.RowType -- Join based on the main UNION
                WHEN 1 THEN lao.LegionellaID
                WHEN 3 THEN lao.LegionellaLocationID
                ELSE lao.LegionellaAssetOutletID
            END =
            CASE lao.RowType -- Join based on this live table
                WHEN 1 THEN lt.LegionellaID
                WHEN 2 THEN lt.LegionellaAssetID
                WHEN 3 THEN lt.LegionellaLocationID
                WHEN 4 THEN lt.LegionellaOutletID
            END
        LEFT JOIN LegionellaFrequencyCategory lfc WITH (NOLOCK) ON lt.LegionellaFrequencyCategoryID = lfc.LegionellaFrequencyCategoryID
        LEFT JOIN LegionellaRiskRating lrr WITH (NOLOCK) ON lt.LegionellaRiskRatingID = lrr.LegionellaRiskRatingID
        LEFT JOIN LegionellaTaskEvent lte WITH (NOLOCK) ON lt.LegionellaTaskID = lte.LegionellaTaskID AND (
            CASE WHEN lfc.DateAddModifier IS NULL
                THEN 1
                ELSE -- Make sure the Recorded date is between the Frequency Category.
                    CASE WHEN lte.Recorded >= dbo.fn_DateAddFromStringPart(lfc.DateAddModifier, -lfc.DateAddValue, @StartDate) AND
                        lte.Recorded < dbo.fn_DateAddFromStringPart(lfc.DateAddModifier, lfc.DateAddValue, @FinishDate)
                        THEN 1
                        ELSE 0
                    END
            END = 1
        ) AND lte.Deleted IS NULL
        LEFT JOIN Employee e WITH (NOLOCK) ON lte.PerformedByEmployeeID = e.EmployeeID
        LEFT JOIN PortalUser pu WITH (NOLOCK) ON lte.PerformedByPortalUserID = pu.PortalUserID
    WHERE
        lt.Deleted IS NULL
    ORDER BY
        lt.LegionellaTaskID,
        lte.Recorded DESC


    -- Start the main SELECT.
    SELECT
        lbt.LegionellaTaskID,
        lbt.RowType,
        lbt.SystemRef,
        lbt.Location,
        lbt.Action,
        ISNULL(lbt.FrequencyCategory, 'Misc.') [FrequencyCategory],
        lbt.FrequencyCategorySortOrder,
        lbt.RiskColour,
        lbt.EventRowNo,
        lbt.Recorded,
        lbt.Comments,
        lbt.PerformedBy
    FROM
        @LogBookTasksData lbt
    WHERE
        lbt.EventRowNo = 1 -- Get the latest Event for each Task.
    ORDER BY
        lbt.IndexID


    SET NOCOUNT OFF;
END
GO

----TASK 1. Add new column for LegionellaOutletSentinel to LegionellaAssetOutletTaskAutoInsert
IF (Select COUNT(*) FROM TEAMS.sys.[all_columns] Where object_id IN (Select object_id FROM TEAMS.sys.tables Where name='LegionellaAssetOutletTaskAutoInsert') AND name='LegionellaOutletSentinel') < 1 BEGIN
  ALTER TABLE TEAMS.dbo.LegionellaAssetOutletTaskAutoInsert
        ADD [LegionellaOutletSentinel] INT  NULL
END
GO

--BEGIN TRAN
      If (SELECT COUNT(*) FROM LegionellaAssetOutletTaskAutoInsert Where LegionellaOutletSentinel IS NOT NULL) = 0 BEGIN
            ----TASK 3. Add 2 new risks for non-sentinels and sentinels with the different frequencies
            DECLARE @MaxLegionellaTaskAutoInsert INT = (Select ISNULL((Select MAX(LegionellaTaskAutoInsertID) FROM LegionellaTaskAutoInsert),0))
            
            SET IDENTITY_INSERT LegionellaTaskAutoInsert ON
            INSERT INTO LegionellaTaskAutoInsert (LegionellaTaskAutoInsertID,LegionellaRiskCategoryID,RiskDescription,Action,LegionellaFrequencyCategoryID,LegionellaRiskRatingID,LegionellaPriorityRatingID)
            SELECT
                  new.ID+@MaxLegionellaTaskAutoInsert [LegionellaTaskAutoInsertID],new.LegionellaRiskCategoryID,new.RiskDescription,new.Action,new.LegionellaFreqencyCategoryID,new.LegionellaRiskRatingID,new.LegionellaPriorityRatingID
            FROM
                  (
                        Select 
                              1 [ID],
                              3 [LegionellaRiskCategoryID],
                              'Take temperature from outlets.' [RiskDescription],
                              'Take temperature from outlets.' [Action],
                              5 [LegionellaFreqencyCategoryID],
                              3 [LegionellaRiskRatingID],
                              4 [LegionellaPriorityRatingID]
                        UNION
                        Select 
                              2 [ID],
                              3 [LegionellaRiskCategoryID],
                              'Take temperature from sentinel outlets.' [RiskDescription],
                              'Take temperature from sentinel outlets.' [Action],
                              3 [LegionellaFreqencyCategoryID],    ----Now monthly
                              3 [LegionellaRiskRatingID],
                              4 [LegionellaPriorityRatingID]
                  ) new
            SET IDENTITY_INSERT LegionellaTaskAutoInsert OFF
            
----TASK 2. Add 3 new rows for LegionellaAssetOutletTaskAutoInsert
      /*
      LegionellaOutletSentinel = 0  Non-sentinel
      LegionellaOutletSentinel = 1  Nearest
      LegionellaOutletSentinel = 2  Furthest
        */
            DECLARE @MaxLegionellaAssetOutletTaskAutoInsert INT = (Select ISNULL((Select MAX(LegionellaAssetOutletTaskAutoInsertID) FROM LegionellaAssetOutletTaskAutoInsert),0))
            
            SET IDENTITY_INSERT LegionellaAssetOutletTaskAutoInsert ON
            INSERT INTO LegionellaAssetOutletTaskAutoInsert (LegionellaAssetOutletTaskAutoInsertID,LegionellaTaskAutoInsertID,LegionellaOutletCategoryID,SortOrder,LegionellaOutletSentinel)
            SELECT
                  new.ID+@MaxLegionellaAssetOutletTaskAutoInsert [LegionellaAssetOutletTaskAutoInsertID],new.LegionellaTaskAutoInsertID,new.LegionellaOutletCategoryID,new.SortOrder,new.LegionellaOutletSentinel
            FROM
                  (
                        Select 
                              1 [ID],
                              @MaxLegionellaTaskAutoInsert+1 [LegionellaTaskAutoInsertID],
                              1 [LegionellaOutletCategoryID],
                              1 [SortOrder],
                              0 [LegionellaOutletSentinel]
                        UNION
                        Select 
                              2 [ID],
                              @MaxLegionellaTaskAutoInsert+2 [LegionellaTaskAutoInsertID],
                              1 [LegionellaOutletCategoryID],
                              1 [SortOrder],
                              1 [LegionellaOutletSentinel]
                        UNION
                        Select 
                              3 [ID],
                              @MaxLegionellaTaskAutoInsert+2 [LegionellaTaskAutoInsertID],
                              1 [LegionellaOutletCategoryID],
                              1 [SortOrder],
                              2 [LegionellaOutletSentinel]
                  ) new
            SET IDENTITY_INSERT LegionellaAssetOutletTaskAutoInsert OFF
        
            --PRINT 'SETUP DATA'    
      END
      
      If (SELECT COUNT(*) FROM LegionellaAssetOutletTaskAutoInsert Where LegionellaOutletSentinel IS NOT NULL) > 0 BEGIN
----TASK 4.
            
      --BACKFILLED ALL EXISTING OUTLETS WITH TASKS BASED ON THEM BEING SENTINELS OR NOT..
            SET NOCOUNT ON;
            DECLARE @HighestTask TABLE (LegionellaID INT, HighestTaskNo INT)
            INSERT INTO @HighestTask (LegionellaID,HighestTaskNo)
            SELECT
                  lao.LegionellaID, MAX(LegionellaTaskNo)
            FROM
                  LegionellaTask lt 
                  INNER JOIN (
                              Select 
                                    l.LegionellaID, la.LegionellaAssetID [LegionellaAssetID], NULL [LegionellaOutletID], NULL [LegionellaLocationID] 
                              FROM 
                                    LegionellaAsset la INNER JOIN Legionella l ON la.LegionellaID=l.LegionellaID 
                              UNION 
                              Select 
                                    l.LegionellaID, NULL [LegionellaAssetID], LegionellaOutletID [LegionellaOutletID], NULL [LegionellaLocationID] 
                              FROM 
                                    LegionellaOutlet lo INNER JOIN LegionellaLocation ll ON lo.LegionellaLocationID=ll.LegionellaLocationID INNER JOIN Legionella l ON ll.LegionellaID=l.LegionellaID 
                              UNION 
                              Select 
                                    l.LegionellaID, NULL [LegionellaAssetID], NULL [LegionellaOutletID], ll.LegionellaLocationID [LegionellaLocationID] 
                              FROM 
                                    LegionellaLocation ll INNER JOIN Legionella l ON ll.LegionellaID=l.LegionellaID 
                              UNION
                              Select
                                    l.LegionellaID, NULL [LegionellaAssetID], NULL [LegionellaOutletID], NULL [LegionellaLocationID] 
                              FROM 
                                    Legionella l
                        ) lao ON (lt.LegionellaID=lao.LegionellaID OR lt.LegionellaAssetID=lao.LegionellaAssetID OR lt.LegionellaOutletID=lao.LegionellaOutletID OR lt.LegionellaLocationID=lao.LegionellaLocationID) 
            GROUP BY
                  lao.LegionellaID
            SET NOCOUNT OFF;
            
            INSERT INTO LegionellaTask (LegionellaTaskAutoInsertID,LegionellaTaskNo,LegionellaOutletID,LegionellaRiskCategoryID,RiskDescription,Action,LegionellaFrequencyCategoryID,LegionellaRiskRatingID,LegionellaPriorityRatingID,SortOrder,GUID,GUIDVersion)
            Select 
                  autoInsert.LegionellaTaskAutoInsertID [LegionellaTaskAutoInsertID],
                  ISNULL(ht.HighestTaskNo,0) + ROW_NUMBER() OVER (Partition By l.LegionellaID Order By lo.LegionellaOutletID ASC) [LegionellaTaskNo],
                  lo.LegionellaOutletID [LegionellaOutletID],
                  autoInsert.LegionellaRiskCategoryID,
                  autoInsert.RiskDescription [RiskDescription],
                  autoInsert.[Action] [Action],
                  autoInsert.LegionellaFrequencyCategoryID [LegionellaFrequencyCategoryID],
                  autoInsert.LegionellaRiskRatingID [LegionellaRiskRatingID],
                  autoInsert.LegionellaPriorityRatingID [LegionellaPriorityRatingID],
                  autoInsert.SortOrder [SortOrder],
                  dbo.CreateGUID(RIGHT('000000000000' + CAST(l.LegionellaID as varchar) + CAST(ISNULL(ht.HighestTaskNo,0) + ROW_NUMBER() OVER (Partition By l.LegionellaID Order By lo.LegionellaOutletID ASC) as varchar),12),'LegionellaTask','TS') [GUID],
                  1 [GUIDVersion]
            FROM 
                  (
                        SELECT
                              sub_lo.LegionellaOutletID,
                              sub_lo.LegionellaLocationID,
                              sub_lo.SentinelCold, sub_lo.SentinelHot, sub_lo.SentinelMains, sub_lo.SentinelMixed
                        FROM
                              LegionellaOutlet sub_lo
                              LEFT OUTER JOIN LegionellaTask lt ON lt.LegionellaOutletID=sub_lo.LegionellaOutletID AND lt.LegionellaTaskAutoInsertID IN (SELECT ltai.LegionellaTaskAutoInsertID FROM LegionellaAssetOutletTaskAutoInsert laotai INNER JOIN LegionellaTaskAutoInsert ltai ON laotai.LegionellaTaskAutoInsertID=ltai.LegionellaTaskAutoInsertID WHERE laotai.LegionellaOutletSentinel IS NOT NULL)
                        WHERE
                              lt.LegionellaTaskID IS NULL
                  ) lo
                  INNER JOIN LegionellaLocation ll ON lo.LegionellaLocationID=ll.LegionellaLocationID
                  INNER JOIN Legionella l ON l.LegionellaID=ll.LegionellaID
                  LEFT OUTER JOIN @HighestTask ht ON ht.LegionellaID = l.LegionellaID
                  CROSS JOIN 
                  (
                        SELECT
                              ltai.LegionellaTaskAutoInsertID,
                              ltai.LegionellaRiskCategoryID [LegionellaRiskCategoryID],
                              ltai.RiskDescription [RiskDescription],
                              ltai.[Action] [Action],
                              ltai.LegionellaFrequencyCategoryID [LegionellaFrequencyCategoryID],
                              ltai.LegionellaRiskRatingID [LegionellaRiskRatingID],
                              ltai.LegionellaPriorityRatingID [LegionellaPriorityRatingID],
                              laotai.SortOrder,
                              laotai.LegionellaOutletSentinel
                        FROM
                              LegionellaAssetOutletTaskAutoInsert laotai
                              INNER JOIN LegionellaTaskAutoInsert ltai ON laotai.LegionellaTaskAutoInsertID=ltai.LegionellaTaskAutoInsertID
                        WHERE
                              laotai.LegionellaOutletSentinel IS NOT NULL
                  ) autoInsert
            WHERE
                  CASE WHEN lo.SentinelCold = 2 OR lo.SentinelHot = 2 OR lo.SentinelMains = 2 OR lo.SentinelMixed = 2 THEN
                        2
                  ELSE
                        CASE WHEN lo.SentinelCold = 1 OR lo.SentinelHot = 1 OR lo.SentinelMains = 1 OR lo.SentinelMixed = 1 THEN
                              1
                        ELSE
                              0
                        END   
                  END = autoInsert.LegionellaOutletSentinel
            ORDER BY
                  l.LegionellaID
                  
      END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'DueDate' AND object_id = object_id('LegionellaTask'))
BEGIN
    ALTER TABLE LegionellaTask
    ADD DueDate DATETIME
END
GO
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaLogBookOutletsWithDueDates')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaLogBookOutletsWithDueDates] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaLogBookOutletsWithDueDates]
    @PortalUserID INT = NULL,
    @ClientIDs VARCHAR(MAX) = NULL,
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL,
    @ThisMonthOnly BIT = 0
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ThisMonthOnly = ISNULL(@ThisMonthOnly, 0)

    -- If @ThisMonthOnly, get the data that is overdue or due this month.
    -- Cast DATETIME to DATE. Add one day to the FinishDate - this will set @FinishDate as the next day but it will be at midnight.
    DECLARE @FinishDate DATETIME
    IF @ThisMonthOnly = 1
    BEGIN
        SET @FinishDate = DATEADD(MONTH, ((YEAR(GETDATE()) - 1900) * 12) + MONTH(GETDATE()), -1)
    END

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    INSERT INTO @SiteIdData (SiteID)
    SELECT LTRIM(RTRIM(s)) [SiteID]
    FROM dbo.SplitString(@SiteIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

	--To limit the data returned to only come from the latest Legionella Risk Assessment job done against the selected site.
	DECLARE @JobIDData TABLE (JobID INT PRIMARY KEY)
	INSERT INTO @JobIDData (JobID)
	SELECT TOP 1
		j.JobID
	FROM
		Job j
		INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
		INNER JOIN @SiteIdData s ON j.SiteID = s.SiteID

		INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
		INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
		INNER JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID

	WHERE al.LegionellaTypeID = 1 AND j.Approved IS NOT NULL
	ORDER BY j.JobID DESC

    -- Get Legionella Outlets data up front to reduce table scans on the Legionella Outlet table.
    DECLARE @LegionellaOutletData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, DateApproved DATETIME NOT NULL, LegionellaLocationID INT NOT NULL, Location VARCHAR(MAX) NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaOutletID INT NOT NULL, OutletSystemRef VARCHAR(8000), OutletGUID VARCHAR(MAX), OutletGUIDVersion INT, OutletEnabledCold BIT NOT NULL, OutletEnabledHot BIT NOT NULL, OutletEnabledMixed BIT NOT NULL, OutletEnabledMains BIT NOT NULL, OutletSentinelType INT NOT NULL)

    INSERT INTO @LegionellaOutletData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, DateApproved, LegionellaLocationID, Location, LocationGUID, LocationGUIDVersion, LegionellaOutletID, OutletSystemRef, OutletGUID, OutletGUIDVersion, OutletEnabledCold, OutletEnabledHot, OutletEnabledMixed, OutletEnabledMains, OutletSentinelType)
    SELECT
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID [LegGUID],
        l.GUIDVersion [LegGUIDVersion],
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion],
        lo.LegionellaOutletID,
        lo.SystemRef [OutletSystemRef],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        lo.EnabledCold [OutletEnabledCold],
        lo.EnabledHot [OutletEnabledHot],
        lo.EnabledMixed [OutletEnabledMixed],
        lo.EnabledMains [OutletEnabledMains],
        CASE
            WHEN lo.SentinelCold = 2 OR lo.SentinelHot = 2 OR lo.SentinelMixed = 2 OR lo.SentinelMains = 2 THEN 2
            WHEN lo.SentinelCold = 1 OR lo.SentinelHot = 1 OR lo.SentinelMixed = 1 OR lo.SentinelMains = 1 THEN 1
            ELSE 0
        END [OutletSentinelType]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
        INNER JOIN @SiteIdData si ON j.SiteID = si.SiteID
		--INNER JOIN @JobIDData jid ON j.JobID = jid.JobID --To limit the data returned to only come from the latest Legionella Risk Assessment job done against the selected site.
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID AND ll.Deleted IS NULL
        INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID AND lo.Deleted IS NULL
    GROUP BY
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID,
        l.GUIDVersion,
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID,
        ll.GUIDVersion,
        lo.LegionellaOutletID,
        lo.SystemRef,
        lo.GUID,
        lo.GUIDVersion,
        lo.EnabledCold,
        lo.EnabledHot,
        lo.EnabledMixed,
        lo.EnabledMains,
        lo.SentinelCold,
        lo.SentinelHot,
        lo.SentinelMixed,
        lo.SentinelMains


    -- Start the main SELECT.
    SELECT
        lo.SiteID,
        lo.JobID,
        lo.LegionellaID,
        lo.DateApproved,
        lo.LegionellaLocationID,
        lo.Location,
        lo.LegionellaOutletID,
        lo.OutletSystemRef,
        lo.OutletSentinelType,
        lte.Recorded [LastTempTaken],
        lt.DueDate [TemperatureNextDue],

        -- Additional columns required by the automatic email feature.
        CASE WHEN NULLIF(lo.Location, '') IS NOT NULL THEN LTRIM(RTRIM(lo.Location)) + ' ' ELSE '' END +
        CASE WHEN NULLIF(lo.OutletSystemRef, '') IS NOT NULL THEN '(' + LTRIM(RTRIM(lo.OutletSystemRef)) + ')' ELSE '' END +
        CASE WHEN lo.OutletSentinelType > 0 THEN ' Sentinel' ELSE '' END [OutletDescription],
        ISNULL(dbo.FormatTeamsDate(lte.Recorded, 1), 'N/A') [LastTempTakenFormatted],
        ISNULL(dbo.FormatTeamsDate(lt.DueDate, 1), 'N/A') [TemperatureNextDueFormatted]
    FROM
        (
            SELECT
                *
            FROM
                (
                    SELECT -- Get each Legionella Outlet record with the max GUID.
                        lo.SiteID,
                        lo.JobID,
                        lo.LegionellaID,
                        lo.LegGUID,
                        lo.LegGUIDVersion,
                        lo.DateApproved,
                        lo.LegionellaLocationID,
                        lo.Location,
                        lo.LocationGUID,
                        lo.LocationGUIDVersion,
                        lo.LegionellaOutletID,
                        lo.OutletSystemRef,
                        lo.OutletGUID,
                        lo.OutletGUIDVersion,
                        lo.OutletEnabledCold,
                        lo.OutletEnabledHot,
                        lo.OutletEnabledMixed,
                        lo.OutletEnabledMains,
                        lo.OutletSentinelType,
                        ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.OutletGUID, NEWID()) ORDER BY lo.OutletGUIDVersion DESC) [RowID]
                    FROM @LegionellaOutletData lo
                ) lo
            WHERE lo.RowID = 1
        ) lo
        LEFT JOIN LegionellaAssetOutletTaskAutoInsert laotai WITH (NOLOCK) ON lo.OutletSentinelType = laotai.LegionellaOutletSentinel AND laotai.Deleted IS NULL
        LEFT JOIN LegionellaTask lt WITH (NOLOCK) ON lo.LegionellaOutletID = lt.LegionellaOutletID AND laotai.LegionellaTaskAutoInsertID = lt.LegionellaTaskAutoInsertID AND lt.Deleted IS NULL -- There should only be one Task per Outlet with a Due Date. Created via mobileTEAMS but updated via the Portal front end.
        LEFT JOIN ( -- Get the latest Task Event for the Due Date task. This stores each Task Due Date, wheras the actual Task record just stores the latest Due Date.
            SELECT
                lte.LegionellaTaskEventID,
                lte.LegionellaTaskID,
                lte.Recorded,
                ROW_NUMBER() OVER (PARTITION BY lte.LegionellaTaskID ORDER BY lte.Recorded DESC) [RowID]
            FROM LegionellaTaskEvent lte
            WHERE lte.Deleted IS NULL
        ) lte ON lt.LegionellaTaskID = lte.LegionellaTaskID AND lte.RowID = 1
    WHERE
        CASE WHEN @ThisMonthOnly = 1
            THEN CASE WHEN lt.DueDate < @FinishDate THEN 1 ELSE 0 END
            ELSE 1
        END = 1
    ORDER BY
        lo.OutletSystemRef,
        lo.Location


    SET NOCOUNT OFF;
END


GO



    SET NOCOUNT OFF;
GO
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaOutletsWithBlankTemps')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaOutletsWithBlankTemps] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaOutletsWithBlankTemps]
    @PortalUserID INT = NULL,
    @StartDate DATETIME = NULL,
    @FinishDate DATETIME = NULL,
    @ClientIDs VARCHAR(MAX) = NULL,
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @StartDate = ISNULL(@StartDate, '01/01/2008'),
        @FinishDate = ISNULL(@FinishDate, GETDATE()),
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), '')

    -- Cast DATETIME to DATE. Add one day to the FinishDate - this will set @FinishDate as the next day but it will be at midnight.
    SET @StartDate  = CAST(@StartDate AS DATE)
    SET @FinishDate = DATEADD(d, 1, CAST(@FinishDate AS DATE))

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Sites table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    INSERT INTO @SiteIdData (SiteID)
    SELECT LTRIM(RTRIM(s)) [SiteID]
    FROM dbo.SplitString(@SiteIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get Legionella Outlets data up front to reduce table scans on the Legionella Outlet table.
    DECLARE @LegionellaOutletData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, DateApproved DATETIME NOT NULL, LegionellaLocationID INT NOT NULL, Location VARCHAR(MAX) NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaOutletID INT NOT NULL, OutletSystemRef VARCHAR(8000), OutletGUID VARCHAR(MAX), OutletGUIDVersion INT, OutletEnabledCold BIT NOT NULL, OutletEnabledHot BIT NOT NULL, OutletEnabledMixed BIT NOT NULL, OutletEnabledMains BIT NOT NULL, OutletSentinelType INT NOT NULL, OutletLowUse BIT NOT NULL)

    INSERT INTO @LegionellaOutletData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, DateApproved, LegionellaLocationID, Location, LocationGUID, LocationGUIDVersion, LegionellaOutletID, OutletSystemRef, OutletGUID, OutletGUIDVersion, OutletEnabledCold, OutletEnabledHot, OutletEnabledMixed, OutletEnabledMains, OutletSentinelType, OutletLowUse)
    SELECT
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID [LegGUID],
        l.GUIDVersion [LegGUIDVersion],
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion],
        lo.LegionellaOutletID,
        lo.SystemRef [OutletSystemRef],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        lo.EnabledCold [OutletEnabledCold],
        lo.EnabledHot [OutletEnabledHot],
        lo.EnabledMixed [OutletEnabledMixed],
        lo.EnabledMains [OutletEnabledMains],
        CASE
            WHEN lo.SentinelCold = 2 OR lo.SentinelHot = 2 OR lo.SentinelMixed = 2 OR lo.SentinelMains = 2 THEN 2
            WHEN lo.SentinelCold = 1 OR lo.SentinelHot = 1 OR lo.SentinelMixed = 1 OR lo.SentinelMains = 1 THEN 1
            ELSE 0
        END [OutletSentinelType],
        CASE WHEN lo.LowUseCold = 1 OR lo.LowUseHot = 1 OR lo.LowUseMixed = 1 OR lo.LowUseMains = 1
            THEN 1
            ELSE 0
        END [OutletLowUse]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
        INNER JOIN @SiteIdData si ON j.SiteID = si.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID AND ll.Deleted IS NULL
        INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID AND lo.Deleted IS NULL
    GROUP BY
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID,
        l.GUIDVersion,
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID,
        ll.GUIDVersion,
        lo.LegionellaOutletID,
        lo.SystemRef,
        lo.GUID,
        lo.GUIDVersion,
        lo.EnabledCold,
        lo.EnabledHot,
        lo.EnabledMixed,
        lo.EnabledMains,
        lo.SentinelCold,
        lo.SentinelHot,
        lo.SentinelMixed,
        lo.SentinelMains,
        lo.LowUseCold,
        lo.LowUseHot,
        lo.LowUseMixed,
        lo.LowUseMains


    -- Start the main SELECT.
    SELECT
        lo.LegionellaOutletID,
        CASE WHEN NULLIF(lo.Location, '') IS NOT NULL THEN LTRIM(RTRIM(lo.Location)) + ' ' ELSE '' END +
        CASE WHEN NULLIF(lo.OutletSystemRef, '') IS NOT NULL THEN '(' + LTRIM(RTRIM(lo.OutletSystemRef)) + ')' ELSE '' END [OutletDescription],
        CASE WHEN lo.OutletEnabledCold = 0 THEN 'N/A' ELSE NULL END [ColdWaterTemp],
        CASE WHEN lo.OutletEnabledHot = 0 THEN 'N/A' ELSE NULL END [HotWaterTemp],
        CASE WHEN lo.OutletEnabledMixed = 0 THEN 'N/A' ELSE NULL END [MixedWaterTemp],
        CASE WHEN lo.OutletEnabledMains = 0 THEN 'N/A' ELSE NULL END [MainsWaterTemp],
        CASE WHEN lo.OutletEnabledCold = 0 THEN 'N/A' ELSE NULL END [ColdFlushed],
        CASE WHEN lo.OutletEnabledHot = 0 THEN 'N/A' ELSE NULL END [HotFlushed],
        CASE WHEN lo.OutletEnabledMixed = 0 THEN 'N/A' ELSE NULL END [MixedFlushed],
        CASE WHEN lo.OutletEnabledMains = 0 THEN 'N/A' ELSE NULL END [MainsFlushed],
        NULL [PerformedByThirdParty],
        NULL [Comments],
        NULL [Recorded]
    FROM
        (
            SELECT
                *
            FROM
                (
                    SELECT -- Get each Legionella Outlet record with the max GUID.
                        lo.SiteID,
                        lo.JobID,
                        lo.LegionellaID,
                        lo.LegGUID,
                        lo.LegGUIDVersion,
                        lo.DateApproved,
                        lo.LegionellaLocationID,
                        lo.Location,
                        lo.LocationGUID,
                        lo.LocationGUIDVersion,
                        lo.LegionellaOutletID,
                        lo.OutletSystemRef,
                        lo.OutletGUID,
                        lo.OutletGUIDVersion,
                        lo.OutletEnabledCold,
                        lo.OutletEnabledHot,
                        lo.OutletEnabledMixed,
                        lo.OutletEnabledMains,
                        lo.OutletSentinelType,
                        ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.OutletGUID, NEWID()) ORDER BY lo.OutletGUIDVersion DESC) [RowID]
                    FROM @LegionellaOutletData lo
                ) lo
            WHERE lo.RowID = 1
        ) lo
        LEFT JOIN LegionellaAssetOutletTaskAutoInsert laotai WITH (NOLOCK) ON lo.OutletSentinelType = laotai.LegionellaOutletSentinel AND laotai.Deleted IS NULL
        LEFT JOIN LegionellaTask lt WITH (NOLOCK) ON lo.LegionellaOutletID = lt.LegionellaOutletID AND laotai.LegionellaTaskAutoInsertID = lt.LegionellaTaskAutoInsertID AND lt.Deleted IS NULL -- There should only be one Task per Outlet with a Due Date. Created via mobileTEAMS but updated via the Portal front end.
        LEFT JOIN ( -- Get the latest Task Event for the Due Date task. This stores each Task Due Date, wheras the actual Task record just stores the latest Due Date.
            SELECT
                lte.LegionellaTaskEventID,
                lte.LegionellaTaskID,
                lte.Recorded,
                ROW_NUMBER() OVER (PARTITION BY lte.LegionellaTaskID ORDER BY lte.Recorded DESC) [RowID]
            FROM LegionellaTaskEvent lte
            WHERE lte.Deleted IS NULL
        ) lte ON lt.LegionellaTaskID = lte.LegionellaTaskID AND lte.RowID = 1
    WHERE
        (lt.DueDate >= @StartDate AND lt.DueDate < @FinishDate)
    ORDER BY
        OutletDescription


    SET NOCOUNT OFF;
END
GO

/*
    AUTOMATIC EMAILS LOGIC.
    CREATE THE NEW TABLES, ROW TEMPLATE, AE INSTRUCTION AND SPROCS.
*/
IF NOT EXISTS(SELECT * FROM sys.objects WHERE name = 'PortalUserAutoEmail')
BEGIN
    CREATE TABLE [dbo].[PortalUserAutoEmail](
	    [PortalUserAutoEmailID] [int] IDENTITY(1,1) NOT NULL,
	    [PortalUserAutoEmailTypeID] [int] NOT NULL,
	    [PortalUserID] [int] NOT NULL,
	    [SiteID] [int] NULL,
     CONSTRAINT [PK_PortalUserAutoEmail] PRIMARY KEY CLUSTERED 
    (
	    [PortalUserAutoEmailID] ASC
    )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
    ) ON [PRIMARY]

    CREATE TABLE [dbo].[PortalUserAutoEmailType](
	    [PortalUserAutoEmailTypeID] [int] NOT NULL,
	    [Description] [varchar](50) NOT NULL,
	    [Deleted] [datetime] NULL,
     CONSTRAINT [PK_PortalUserAutoEmailType] PRIMARY KEY CLUSTERED 
    (
	    [PortalUserAutoEmailTypeID] ASC
    )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
    ) ON [PRIMARY]

    CREATE UNIQUE NONCLUSTERED INDEX [Idx_PortalUserAutoEmail_TypeUserSite] ON [dbo].[PortalUserAutoEmail] 
    (
	    [PortalUserAutoEmailTypeID] ASC,
	    [PortalUserID] ASC,
	    [SiteID] ASC
    )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]

    CREATE NONCLUSTERED INDEX [Idx_PortalUserAutoEmailType_Deleted] ON [dbo].[PortalUserAutoEmailType] 
    (
	    [Deleted] ASC
    )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]

    INSERT INTO PortalUserAutoEmailType (PortalUserAutoEmailTypeID, Description)
    VALUES (1, 'Legionella Outlet Temperature Monitoring Schedule')
END

IF NOT EXISTS(SELECT * FROM RowTemplate WHERE ReplaceVariable = 'AEPORTALLEGOUTLETTEMPMONITORINGSCHEDULE')
BEGIN
    INSERT INTO RowTemplate (ReplaceVariable, HeaderHTML, RowHTML, FooterHTML, HeaderFooterSQL, RowSQL, NoDataRowHTML, NoDataRowSQL, MaxRowsPerPage, MaxRowsPerPageAdditionalPagesOverride, RowTemplateTypeInt)
    VALUES ('AEPORTALLEGOUTLETTEMPMONITORINGSCHEDULE', '<!-- No Header -->', '<!-- No Row -->', '<!-- No Footer -->', 'SELECT 0', 'SELECT 0', '<!-- No Data Row -->', 'SELECT 0', 0, 0, 3)
END

UPDATE RowTemplate
SET
    HeaderHTML = '<!-- Header -->
<p style="font-weight: bold; font-family: Arial; font-size: 11pt;">[EmailDate] - [SiteAddressWithPostcode]</p>
<p style="font-weight: bold; font-family: Arial; font-size: 11pt; margin-top: 30px;">Legionella Outlet Temperature Monitoring Schedule</p>
<table style="border-collapse: collapse; width: 100%;">
<tr>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Location and System Ref</th>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Last Temp Taken</th>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Temperature Next Due</th>
</tr>
<!-- End Header -->',
    RowHTML = '<!-- Row -->
<tr>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[OutletDescription]</td>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[LastTempTakenFormatted]</td>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[TemperatureNextDueFormatted]</td>
</tr>
<!-- End Row -->',
    FooterHTML = '<!-- Footer -->
</table>
<!-- End Footer -->',
    HeaderFooterSQL = 'DECLARE @SiteID INT = [SiteID]  SELECT dbo.FormatTeamsDate(GETDATE(), 1) ''EmailDate'', si.Address + ISNULL('', '' + NULLIF(si.Postcode, ''''), '''') ''SiteAddressWithPostcode''  FROM Site si WITH (NOLOCK)  WHERE si.SiteID = @SiteID',
    RowSQL = 'EXEC GetPortalLegionellaLogBookOutletsWithDueDates @PortalUserID = [PortalUserID], @ClientIDs = ''[ClientIDs]'', @ProjectGroupID = [ProjectGroupID], @ProjectID = [ProjectID], @SiteIDs = ''[SiteID]'', @ThisMonthOnly = 1',
    NoDataRowHTML = '<!-- No Data Row -->
<tr>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px;" colspan="3">No Outlet Temperature Monitoring Schedule data found.</td>
</tr>
<!-- End No Data Row -->',
    NoDataRowSQL = 'SELECT 0',
    MaxRowsPerPage = 0,
    MaxRowsPerPageAdditionalPagesOverride = 0,
    RowTemplateTypeInt = 3
WHERE ReplaceVariable = 'AEPORTALLEGOUTLETTEMPMONITORINGSCHEDULE'

IF NOT EXISTS(SELECT * FROM RowTemplate WHERE ReplaceVariable = 'AEPORTALLEGOUTLETFLUSHINGLOWUSE')
BEGIN
    INSERT INTO RowTemplate (ReplaceVariable, HeaderHTML, RowHTML, FooterHTML, HeaderFooterSQL, RowSQL, NoDataRowHTML, NoDataRowSQL, MaxRowsPerPage, MaxRowsPerPageAdditionalPagesOverride, RowTemplateTypeInt)
    VALUES ('AEPORTALLEGOUTLETFLUSHINGLOWUSE', '<!-- No Header -->', '<!-- No Row -->', '<!-- No Footer -->', 'SELECT 0', 'SELECT 0', '<!-- No Data Row -->', 'SELECT 0', 0, 0, 3)
END

UPDATE RowTemplate
SET
    HeaderHTML = '<!-- Header -->
<p style="font-weight: bold; font-family: Arial; font-size: 11pt; margin-top: 30px;">Legionella Flushing Records for Low Use Outlets</p>
<table style="border-collapse: collapse; width: 100%;">
<tr>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Location and System Ref</th>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Last Flushing Record</th>
    <th style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">Flushing Next Due</th>
</tr>
<!-- End Header -->',
    RowHTML = '<!-- Row -->
<tr>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[OutletDescription]</td>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[LastFlushingRecordFormatted]</td>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px; width: 33%;">[FlushingNextDueFormatted]</td>
</tr>
<!-- End Row -->',
    FooterHTML = '<!-- Footer -->
</table>
<!-- End Footer -->',
    HeaderFooterSQL = 'SELECT 0',
    RowSQL = 'EXEC GetPortalLegionellaOutletsFlushingLowUse @PortalUserID = [PortalUserID], @ClientIDs = ''[ClientIDs]'', @ProjectGroupID = [ProjectGroupID], @ProjectID = [ProjectID], @SiteIDs = ''[SiteID]'', @ThisMonthOnly = 1',
    NoDataRowHTML = '<!-- No Data Row -->
<tr>
    <td style="font-family: Arial; font-size: 11pt; border: 1px solid #000000; padding: 3px 5px;" colspan="3">No Legionella Flushing Records for Low Use Outlets found.</td>
</tr>
<!-- End No Data Row -->',
    NoDataRowSQL = 'SELECT 0',
    MaxRowsPerPage = 0,
    MaxRowsPerPageAdditionalPagesOverride = 0,
    RowTemplateTypeInt = 3
WHERE ReplaceVariable = 'AEPORTALLEGOUTLETFLUSHINGLOWUSE'

IF NOT EXISTS(SELECT * FROM aeInstruction WHERE EmailSubject = 'Outlet Temperature Monitoring Schedule [EmailDate] for [SiteAddressWithPostcode]')
BEGIN
    INSERT INTO aeInstruction (EmailRecipient, EmailSubject, EmailBody, SQL, orgCode, TimeBetweenStart, TimeBetweenEnd, ExecOnDays, DateLastRun)
    VALUES ('[EMAILRECIPIENT]', 'Outlet Temperature Monitoring Schedule [EmailDate] for [SiteAddressWithPostcode]', '', '', '', '', '', '', DATEADD(day, -1, GETDATE()))
END

UPDATE aeInstruction
SET
    EmailRecipient = '[EMAILRECIPIENT]',
    EmailBody = '[AEPORTALLEGOUTLETTEMPMONITORINGSCHEDULE]<br />[AEPORTALLEGOUTLETFLUSHINGLOWUSE]',
    SQL = 'EXEC AutomaticEmail_PortalUserForSite @PortalUserAutoEmailTypeID = 1',
    orgCode = 'TS',
    TimeBetweenStart = '06:00:00.0000000',
    TimeBetweenEnd = '06:10:00.0000000',
    ExecOnDays = '1',
    DateLastRun = DATEADD(day, -1, GETDATE())
WHERE EmailSubject = 'Outlet Temperature Monitoring Schedule [EmailDate] for [SiteAddressWithPostcode]'

IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'AutomaticEmail_PortalUserForSite')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[AutomaticEmail_PortalUserForSite] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[AutomaticEmail_PortalUserForSite]
    @PortalUserAutoEmailTypeID INT = NULL
/*
    For the Portal Automatic Email functionality, get the data that the Client can access. However, we only get the SiteID if we are want emails for this Site.
*/
AS
BEGIN
    SET NOCOUNT ON;


    -- Start the main SELECT.
    SELECT
        pu.PortalUserId [PortalUserID],
        pu.Email + ';' + pu.FullName [EMAILRECIPIENT],
        dbo.FormatTeamsDate(GETDATE(), 1) [EmailDate],
        si.Address + ISNULL(', ' + NULLIF(si.Postcode, ''), '') [SiteAddressWithPostcode],
        c.ClientIDs,
        ISNULL(pu.ProjectGroupID, 0) [ProjectGroupID],
        ISNULL(pu.ProjectID, 0) [ProjectID],
        si.SiteID [SiteID]
    FROM
        PortalUserAutoEmail puae WITH (NOLOCK)
        INNER JOIN PortalUserAutoEmailType puaet WITH (NOLOCK) ON puae.PortalUserAutoEmailTypeID = puaet.PortalUserAutoEmailTypeID
        INNER JOIN PortalUser pu WITH (NOLOCK) ON puae.PortalUserID = pu.PortalUserId AND pu.Deleted IS NULL
        INNER JOIN Site si WITH (NOLOCK) ON puae.SiteID = si.SiteID
        OUTER APPLY
        (
            SELECT STUFF((
                SELECT ',' + CAST(c.ClientID AS VARCHAR(50))
                FROM
                    ClientPortalUser cpu WITH (NOLOCK)
                    INNER JOIN Client c WITH (NOLOCK) ON cpu.ClientId = c.ClientID
                WHERE cpu.PortalUserId = pu.PortalUserId AND c.Deleted IS NULL
                GROUP BY c.ClientID
            FOR XML PATH(''), TYPE).value('.', 'VARCHAR(MAX)'), 1, 1, '') [ClientIDs]
        ) c
    WHERE
        puae.PortalUserAutoEmailTypeID = @PortalUserAutoEmailTypeID
            AND
        puaet.Deleted IS NULL
            AND
        pu.Email LIKE '%@%' -- Add basic validation to make sure it's an email.


    SET NOCOUNT OFF;
END
GO
IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaOutletsFlushingLowUse')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaOutletsFlushingLowUse] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaOutletsFlushingLowUse]
    @PortalUserID INT = NULL,
    @ClientIDs VARCHAR(MAX) = NULL,
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL,
    @ThisMonthOnly BIT = 0
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ThisMonthOnly = ISNULL(@ThisMonthOnly, 0)

    -- If @ThisMonthOnly, get the data that is overdue or due this month.
    -- Cast DATETIME to DATE. Add one day to the FinishDate - this will set @FinishDate as the next day but it will be at midnight.
    DECLARE @FinishDate DATETIME
    IF @ThisMonthOnly = 1
    BEGIN
        SET @FinishDate = DATEADD(MONTH, ((YEAR(GETDATE()) - 1900) * 12) + MONTH(GETDATE()), -1)
    END

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    INSERT INTO @SiteIdData (SiteID)
    SELECT LTRIM(RTRIM(s)) [SiteID]
    FROM dbo.SplitString(@SiteIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get Legionella Outlets data up front to reduce table scans on the Legionella Outlet table.
    DECLARE @LegionellaOutletData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, DateApproved DATETIME NOT NULL, LegionellaLocationID INT NOT NULL, Location VARCHAR(MAX) NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaOutletID INT NOT NULL, OutletSystemRef VARCHAR(8000), OutletGUID VARCHAR(MAX), OutletGUIDVersion INT, OutletEnabledCold BIT NOT NULL, OutletEnabledHot BIT NOT NULL, OutletEnabledMixed BIT NOT NULL, OutletEnabledMains BIT NOT NULL, OutletSentinelType INT NOT NULL, OutletLowUse BIT NOT NULL)

    INSERT INTO @LegionellaOutletData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, DateApproved, LegionellaLocationID, Location, LocationGUID, LocationGUIDVersion, LegionellaOutletID, OutletSystemRef, OutletGUID, OutletGUIDVersion, OutletEnabledCold, OutletEnabledHot, OutletEnabledMixed, OutletEnabledMains, OutletSentinelType, OutletLowUse)
    SELECT
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID [LegGUID],
        l.GUIDVersion [LegGUIDVersion],
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion],
        lo.LegionellaOutletID,
        lo.SystemRef [OutletSystemRef],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        lo.EnabledCold [OutletEnabledCold],
        lo.EnabledHot [OutletEnabledHot],
        lo.EnabledMixed [OutletEnabledMixed],
        lo.EnabledMains [OutletEnabledMains],
        CASE
            WHEN lo.SentinelCold = 2 OR lo.SentinelHot = 2 OR lo.SentinelMixed = 2 OR lo.SentinelMains = 2 THEN 2
            WHEN lo.SentinelCold = 1 OR lo.SentinelHot = 1 OR lo.SentinelMixed = 1 OR lo.SentinelMains = 1 THEN 1
            ELSE 0
        END [OutletSentinelType],
        CASE WHEN lo.LowUseCold = 1 OR lo.LowUseHot = 1 OR lo.LowUseMixed = 1 OR lo.LowUseMains = 1
            THEN 1
            ELSE 0
        END [OutletLowUse]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
        INNER JOIN @SiteIdData si ON j.SiteID = si.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID AND ll.Deleted IS NULL
        INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID AND lo.Deleted IS NULL
    GROUP BY
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID,
        l.GUIDVersion,
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID,
        ll.GUIDVersion,
        lo.LegionellaOutletID,
        lo.SystemRef,
        lo.GUID,
        lo.GUIDVersion,
        lo.EnabledCold,
        lo.EnabledHot,
        lo.EnabledMixed,
        lo.EnabledMains,
        lo.SentinelCold,
        lo.SentinelHot,
        lo.SentinelMixed,
        lo.SentinelMains,
        lo.LowUseCold,
        lo.LowUseHot,
        lo.LowUseMixed,
        lo.LowUseMains


    -- Start the main SELECT.
    SELECT
        *
    FROM
    (
        SELECT
            lo.SiteID,
            lo.JobID,
            lo.LegionellaID,
            lo.DateApproved,
            lo.LegionellaLocationID,
            lo.Location,
            lo.LegionellaOutletID,
            lo.OutletSystemRef,
            lo.OutletSentinelType,
            lo.Recorded [LastFlushingRecord],
            DATEADD(WEEK, 1, lo.Recorded) [FlushingNextDue],

            -- Additional columns required by the automatic email feature.
            CASE WHEN NULLIF(lo.Location, '') IS NOT NULL THEN LTRIM(RTRIM(lo.Location)) + ' ' ELSE '' END +
            CASE WHEN NULLIF(lo.OutletSystemRef, '') IS NOT NULL THEN '(' + LTRIM(RTRIM(lo.OutletSystemRef)) + ')' ELSE '' END +
            CASE WHEN lo.OutletSentinelType > 0 THEN ' Sentinel' ELSE '' END [OutletDescription],
            ISNULL(dbo.FormatTeamsDate(lo.Recorded, 1), 'N/A') [LastFlushingRecordFormatted],
            ISNULL(dbo.FormatTeamsDate(DATEADD(WEEK, 1, lo.Recorded), 1), 'N/A') [FlushingNextDueFormatted]
        FROM
            (
                SELECT
                    *
                FROM
                    (
                        SELECT -- Get each Legionella Outlet record with the max GUID.
                            lo.SiteID,
                            lo.JobID,
                            lo.LegionellaID,
                            lo.LegGUID,
                            lo.LegGUIDVersion,
                            lo.DateApproved,
                            lo.LegionellaLocationID,
                            lo.Location,
                            lo.LocationGUID,
                            lo.LocationGUIDVersion,
                            lo.LegionellaOutletID,
                            lo.OutletSystemRef,
                            lo.OutletGUID,
                            lo.OutletGUIDVersion,
                            lo.OutletEnabledCold,
                            lo.OutletEnabledHot,
                            lo.OutletEnabledMixed,
                            lo.OutletEnabledMains,
                            lo.OutletSentinelType,
                            MAX(locd.Recorded) [Recorded],
                            ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.OutletGUID, NEWID()) ORDER BY lo.OutletGUIDVersion DESC) [RowID]
                        FROM @LegionellaOutletData lo
                        INNER JOIN LegionellaOutletComputedData locd WITH (NOLOCK) ON lo.LegionellaOutletID = locd.LegionellaOutletID
                        WHERE locd.LowUseCold = 1 OR locd.LowUseHot = 1 OR locd.LowUseMixed = 1 OR locd.LowUseMains = 1
                        GROUP BY
                            lo.SiteID,
                            lo.JobID,
                            lo.LegionellaID,
                            lo.LegGUID,
                            lo.LegGUIDVersion,
                            lo.DateApproved,
                            lo.LegionellaLocationID,
                            lo.Location,
                            lo.LocationGUID,
                            lo.LocationGUIDVersion,
                            lo.LegionellaOutletID,
                            lo.OutletSystemRef,
                            lo.OutletGUID,
                            lo.OutletGUIDVersion,
                            lo.OutletEnabledCold,
                            lo.OutletEnabledHot,
                            lo.OutletEnabledMixed,
                            lo.OutletEnabledMains,
                            lo.OutletSentinelType
                    ) lo
                WHERE lo.RowID = 1
            ) lo
    ) a
    WHERE
        CASE WHEN @ThisMonthOnly = 1
            THEN CASE WHEN a.FlushingNextDue < @FinishDate THEN 1 ELSE 0 END
            ELSE 1
        END = 1
    ORDER BY
        a.OutletSystemRef,
        a.Location


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaSurveys]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @PropertyTypeID INT = NULL,
    @LegionellaTypeID INT = NULL,
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @JobNo INT = 0,
    @JobNoSearch INT = 0,
    @GetSiteDocuments INT = 0 /* 0 or NULL = Any, 1 = Yes, 2 = No */
/**********************************************************************
** Overview: Get a filtered collection of Legionella Surveys.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @PropertyTypeID = NULLIF(@PropertyTypeID, 0),
        @LegionellaTypeID = NULLIF(@LegionellaTypeID, 0),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @JobNo = NULLIF(@JobNo, 0),
        @JobNoSearch = NULLIF(@JobNoSearch, 0),
        @GetSiteDocuments = ISNULL(@GetSiteDocuments, 0)

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocPropertyTypeID INT = @PropertyTypeID,
        @LocLegionellaTypeID INT = @LegionellaTypeID,
        @LocClientOrderNo VARCHAR(50) = @ClientOrderNo,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocJobNo INT = @JobNo,
        @LocJobNoSearch INT = @JobNoSearch,
        @LocGetSiteDocuments INT = @GetSiteDocuments

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END

    -- Get all Legionella Survey Data up front to reduce the main SELECT table scans.
    DECLARE @LegionellaData TABLE (IsSiteDocument BIT NOT NULL, JobID INT, JobNo INT, Created DATETIME NOT NULL, Approved DATETIME, LastNoteCreated DATETIME, FileName VARCHAR(100), ReportVersions INT NOT NULL, SiteID INT NOT NULL, Address VARCHAR(200) NOT NULL, Postcode VARCHAR(10) NOT NULL, UPRN VARCHAR(50), LegionellaStartDate DATETIME NOT NULL, LegionellaFinishDate DATETIME, GeneralDescriptionOfSite VARCHAR(MAX), LegionellaTypeID INT, LegionellaType VARCHAR(MAX), SiteDocumentID INT)

    -- Get normal TEAMS Legionella Surveys first.
    IF @LocGetSiteDocuments <> 1
    BEGIN
        INSERT INTO @LegionellaData (IsSiteDocument, JobID, JobNo, Created, Approved, LastNoteCreated, FileName, ReportVersions, SiteID, Address, Postcode, UPRN, LegionellaStartDate, LegionellaFinishDate, GeneralDescriptionOfSite, LegionellaTypeID, LegionellaType, SiteDocumentID)
        SELECT
            0 [IsSiteDocument],
            j.JobID,
            j.JobNo,
            j.Created,
            j.Approved,
            MAX(n.DateCreated) [LastNoteCreated],
            pdfFile.FileName [FileName],
            pdfVersions.ReportVersions [ReportVersions],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            CAST(MIN(l.LegionellaStart) AS DATE) [LegionellaStartDate],
            CAST(MIN(l.LegionellaFinish) AS DATE) [LegionellaFinishDate],
            MAX(l.GeneralDescriptionOfSite) [GeneralDescriptionOfSite],
            lt.LegionellaTypeID,
            lt.Description [LegionellaType],
            NULL [SiteDocumentID]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN #ClientIdData cid ON j.ClientID = cid.ClientID
            LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
            INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
            INNER JOIN AppointmentLegionella al WITH (NOLOCK) ON a.AppointmentID = al.AppointmentID
            INNER JOIN LegionellaType lt WITH (NOLOCK) ON al.LegionellaTypeID = lt.LegionellaTypeID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
            LEFT JOIN Note n WITH (NOLOCK) ON j.JobID = n.ItemID AND n.NoteTypeID = 3 AND n.PortalUserID IS NOT NULL
            OUTER APPLY
            (
                SELECT TOP 1 _pf.FileName [FileName]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%bsr%'
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName NOT LIKE '%asb5%'
                ORDER BY
                    _pf.DateCreated DESC
            ) pdfFile
            OUTER APPLY
            (
                SELECT COUNT(_pf.FileName) [ReportVersions]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%bsr%'
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName NOT LIKE '%asb5%'
            ) pdfVersions
        WHERE -- Some of these use a CASE for short circuiting purposes.
            si.Deleted IS NULL
                AND
            j.Cancelled IS NULL
                AND
            j.Approved IS NOT NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocPropertyTypeID IS NULL -- Property Type Filter.
                THEN 1
                ELSE CASE WHEN al.PropertyTypeID = @LocPropertyTypeID THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocLegionellaTypeID IS NULL -- Legionella Type Filter.
                THEN 1
                ELSE CASE WHEN lt.LegionellaTypeID = @LocLegionellaTypeID THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocClientOrderNo IS NULL -- Client Order No Filter.
                THEN 1
                ELSE CASE WHEN j.ClientOrderNo = @LocClientOrderNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                THEN 1
                ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                THEN 1
                ELSE
                    CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR j.ClientOrderNo = @LocAddressSearchString
                        THEN 1
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocJobNo IS NULL -- Job Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocJobNoSearch IS NULL -- Job Search Filter.
                THEN 1
                ELSE CASE WHEN CAST(j.JobNo AS VARCHAR(50)) LIKE CAST(@LocJobNoSearch AS VARCHAR(50)) + '%' THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            j.JobID,
            j.JobNo,
            j.Created,
            j.Approved,
            pdfFile.FileName,
            pdfVersions.ReportVersions,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            lt.LegionellaTypeID,
            lt.Description
        ORDER BY
            j.Approved DESC
    END

    -- Get the Site Documents.
    IF @LocGetSiteDocuments <> 2
    BEGIN
        INSERT INTO @LegionellaData (IsSiteDocument, JobID, JobNo, Created, Approved, LastNoteCreated, FileName, ReportVersions, SiteID, Address, Postcode, UPRN, LegionellaStartDate, LegionellaFinishDate, GeneralDescriptionOfSite, LegionellaTypeID, LegionellaType, SiteDocumentID)
        SELECT
            1 [IsSiteDocument],
            NULL [JobID],
            NULL [JobNo],
            sid.Uploaded [Created],
            NULL [Approved],
            NULL [LastNoteCreated],
            sid.FileName,
            sidc.SiteDocumentCount [ReportVersions],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            sid.WorkDate [LegionellaStartDate],
            sid.WorkDate [LegionellaFinishDate],
            NULL [LegionellaTypeID],
            NULL [LegionellaType],
            NULL [GeneralDescriptionOfSite],
            sid.SiteDocumentID
        FROM
            Site si WITH (NOLOCK)
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
            INNER JOIN #ClientIdData cid ON cs.ClientID = cid.ClientID
            OUTER APPLY
            (
                SELECT TOP 1
                    sid.SiteDocumentID,
                    sid.SiteID,
                    sid.EmployeeID,
                    sid.PortalUserID,
                    sid.FileName,
                    sid.Uploaded,
                    sidi.SiteDocumentInformationID,
                    sidi.WorkDate
                FROM
                    SiteDocument sid WITH (NOLOCK)
                    INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentID = sidi.SiteDocumentID
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 6
                ORDER BY
                    sid.Uploaded DESC
            ) sid
            OUTER APPLY
            (
                SELECT COUNT(*) [SiteDocumentCount]
                FROM SiteDocument sid WITH (NOLOCK)
                WHERE
                    sid.SiteID = si.SiteID
                        AND
                    sid.Deleted IS NULL
                        AND
                    sid.SiteDocumentTypeID = 6
            ) sidc
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            sid.SiteDocumentID IS NOT NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            sid.SiteDocumentID,
            sid.FileName,
            sid.Uploaded,
            sid.WorkDate,
            sidc.SiteDocumentCount
        ORDER BY
            sid.Uploaded DESC
    END


    -- Start the main SELECT
    SELECT *
    FROM @LegionellaData
    ORDER BY
        ISNULL(Approved, Created) DESC,
        IsSiteDocument

    -- Clear up temp tables.
    DROP TABLE #ClientIdData


    SET NOCOUNT OFF;
END
GO
ALTER PROCEDURE [dbo].[GetQuotes]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @QuoteNo INT = 0,
    @QuoteNoSearch INT = 0
/**********************************************************************
** Overview: Get a filtered collection of Quotes.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @QuoteNo = NULLIF(@QuoteNo, 0),
        @QuoteNoSearch = NULLIF(@QuoteNoSearch, 0)

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocQuoteNo INT = @QuoteNo,
        @LocQuoteNoSearch INT = @QuoteNoSearch

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END

    -- Start the main SELECT
    SELECT
        q.QuoteID,
        q.QuoteNo,
        q.Value,
        q.Created,
        ISNULL(q.Status, '') [Status],
        q.Accepted,
        q.ClientOrderNo,
        qt.QuoteTypeID,
        qt.QuoteType,
        pf.FileName [QuoteFileName],
        si.SiteID,
        si.Address [SiteAddress],
        si.Postcode [SitePostcode],
        si.UPRN [SiteUPRN]
    FROM
        Quote q WITH (NOLOCK)
        INNER JOIN QuoteType qt WITH (NOLOCK) ON q.QuoteTypeID = qt.QuoteTypeID
        INNER JOIN #ClientIdData cid ON q.ClientID = cid.ClientID
        LEFT JOIN Project p WITH (NOLOCK) ON q.ProjectID = p.ProjectID
        LEFT JOIN Site si WITH (NOLOCK) ON q.SiteID = si.SiteID
        LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
        OUTER APPLY
        (
            SELECT TOP 1 _pf.PdfID, _pf.FileName
            FROM PDF _pf WITH (NOLOCK)
            WHERE
                _pf.QuoteID = q.QuoteID
                    AND
                _pf.DateDeleted IS NULL
            ORDER BY
                _pf.DateCreated DESC
        ) pf
    WHERE -- Some of these use a CASE for short circuiting purposes.
        si.Deleted IS NULL
            AND
        p.Deleted IS NULL
            AND
        q.Rejected IS NULL -- Exclude Rejected quotes
            AND
        pf.PdfID > 0 -- Only show Quotes that have a PDF (hiding Instaquotes)
            AND
        CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
            THEN 1
            ELSE
                CASE WHEN p.Deleted IS NULL
                    THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
            THEN 1
            ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
            THEN 1
            ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
            THEN 1
            ELSE
                CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR q.ClientOrderNo = @LocAddressSearchString
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @LocQuoteNo IS NULL -- Quote Filter.
            THEN 1
            ELSE CASE WHEN q.QuoteNo = @LocQuoteNo THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocQuoteNoSearch IS NULL -- Quote Search Filter.
            THEN 1
            ELSE CASE WHEN CAST(q.QuoteNo AS VARCHAR(50)) LIKE CAST(@LocQuoteNoSearch AS VARCHAR(50)) + '%' THEN 1 ELSE 0 END
        END = 1
    ORDER BY
        q.Created DESC

    -- Clear up temp tables.
    DROP TABLE #ClientIdData


    SET NOCOUNT OFF;
END
GO

-- Portal Super User
IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'SuperUser' AND object_id = OBJECT_ID('PortalUser'))
BEGIN
    ALTER TABLE PortalUser
    ADD SuperUser BIT NOT NULL DEFAULT 0
END
GO

-- Insert a portal super user
IF NOT EXISTS(SELECT * FROM PortalUser WHERE [Company]='Super User' AND [FullName]='Super User' AND [SuperUser]=1)
BEGIN
	INSERT INTO [dbo].[PortalUser] ([Company],[FullName],[Email],[Password],[Enabled],[Diary],[Quotes],[Surveys]
		,[Projects],[AirTests],[Removals],[BulkSamples],[Invoices],[Sites],[Activity],[Created],[FailedLoginAttempts]
		,[UseSiteGroups],[ProjectGroupId],[ProjectId],[AccessGUID],[PremiumUser],[CookieAccess],[PortalAdministrator]
		,[Legionella],[Training],[Maps],[Home],[Glance],[TotalComplianceChart],[LegTotalComplianceChart],[SitesSurveyedChart]
		,[RiskItemsChart],[LegRiskItemsChart],[RecommendedActionItemsChart],[TargetTimescalesChart],[TrainingComplianceChart]
		,[LegLowUseOutletsChart],[LegOutletsOutOfSpecChart],[SuperUser])
	VALUES
		('Super User','Super User','superuser@m1c.co.uk',(SELECT Password FROM Employee WHERE Username='swetherell'),1,1,1,1,1
		,1,1,1,1,1,1,GETDATE(),0,1,0,0,NEWID(),1,NEWID(),1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'b__EnableEnterpriseTools' AND object_id = OBJECT_ID('Config'))
BEGIN
    ALTER TABLE Config
    ADD b__EnableEnterpriseTools BIT NOT NULL DEFAULT 0
END
GO

IF NOT EXISTS(SELECT * FROM SiteDocumentType WHERE Description = 'Site Photo')
BEGIN
    INSERT INTO SiteDocumentType (SiteDocumentTypeID, Description, SortOrder, [Default])
    VALUES (7, 'Site Photo', 7, 0)
END
GO

ALTER VIEW [dbo].[PortalUserNotes]
AS

-- Notes for a Portal User.
SELECT
    n.NoteId,
    ISNULL(n.Note, '') [Note],
    n.DateCreated,
    nt.NoteType,
    n.ItemId,
    CASE nt.NoteType
        WHEN 'Quote' THEN
            (
                SELECT dbo.FormatTeamsReference('Q', QuoteNo)
                FROM Quote WITH (NOLOCK)
                WHERE QuoteID = n.ItemID
            )
        WHEN 'Job' THEN
            (
                SELECT dbo.FormatTeamsReference('J', JobNo)
                FROM Job WITH (NOLOCK)
                WHERE JobID = n.ItemID
            )
        WHEN 'AirTest' THEN
            (
                SELECT
                    dbo.FormatTeamsReference('J', _j.JobNo) +
                    CASE WHEN _at.AirTestNo > 0
                        THEN ' / ' + ISNULL(_at.SystemName,'') + RIGHT('0' + CAST(_at.AirTestNo AS VARCHAR(100)), 2)
                        ELSE ''
                    END
                FROM
                    AirTest _at WITH (NOLOCK)
                    INNER JOIN JobEmployee _je WITH (NOLOCK) ON _at.JobEmployeeID = _je.JobEmployeeID
                    INNER JOIN Job _j WITH (NOLOCK) ON _je.JobID = _j.jobid
                WHERE
                    _at.AirTestID = n.ItemID
            )
        WHEN 'Invoice' THEN
            (
                SELECT dbo.FormatTeamsReference('I', InvoiceNo)
                FROM Invoice WITH (NOLOCK)
                WHERE InvoiceID = n.ItemID
            )
    END [ItemNo],
    pu.FullName [PortalUser],
    pu.Company,
    c.ClientId,
    c.Client
FROM
    Note n WITH (NOLOCK)
    INNER JOIN NoteType nt WITH (NOLOCK) ON n.NoteTypeID = nt.NoteTypeID
    INNER JOIN PortalUser pu WITH (NOLOCK) ON n.PortalUserID = pu.PortalUserID
    LEFT JOIN Client c WITH (NOLOCK) ON pu.ClientID = c.ClientID -- Invalid now, but kept in for the old Portal

UNION

-- No Access for a Job.
SELECT
    NULL [NoteId],
    CASE WHEN p.ContentType IS NULL
        THEN ''
        ELSE '<img src="/File/GetPhoto?id=' + CAST(p.PhotoID AS VARCHAR(100)) + '&width=150" />' + CHAR(13)+CHAR(10)
    END + na.Comment [Note],
    na.Created [DateCreated],
    'Site' [NoteType],
    j.SiteID [ItemId],
    dbo.FormatTeamsReference('J', j.JobNo) [ItemNo],
    e.FullName [PortalUser],
    cfg.s__CompanyName [Company],
    NULL [ClientId],
    NULL [Client]
FROM
    NoAccess na WITH (NOLOCK)
    LEFT JOIN Photo p WITH (NOLOCK) ON na.PhotoID = p.PhotoID AND p.ContentType IS NOT NULL
    INNER JOIN Job j WITH (NOLOCK) ON na.JobID = j.JobID
    INNER JOIN Employee e WITH (NOLOCK) ON na.EmployeeID = e.EmployeeID
    CROSS JOIN Config cfg WITH (NOLOCK)

UNION

-- No Access for a Site.
SELECT
    NULL [NoteId],
    CASE WHEN p.ContentType IS NULL
        THEN ''
        ELSE '<img src="/File/GetPhoto?id=' + CAST(p.PhotoID AS VARCHAR(100)) + '&width=150" />' + CHAR(13)+CHAR(10)
    END + na.Comment [Note],
    na.Created [DateCreated],
    'Site' [NoteType],
    si.SiteID [ItemId],
    '' [ItemNo],
    e.FullName [PortalUser],
    cfg.s__CompanyName [Company],
    NULL [ClientId],
    NULL [Client]
FROM
    NoAccess na WITH (NOLOCK)
    LEFT JOIN Photo p WITH (NOLOCK) ON na.PhotoID = p.PhotoID AND p.ContentType IS NOT NULL
    INNER JOIN Site si WITH (NOLOCK) ON na.SiteID = si.SiteID
    INNER JOIN Employee e WITH (NOLOCK) ON na.EmployeeID = e.EmployeeID
    CROSS JOIN Config cfg WITH (NOLOCK)

UNION

-- Air Test Photos from the Site Log.
SELECT
    NULL [NoteId],
    CASE WHEN p.ContentType IS NULL
        THEN ''
        ELSE '<img src="/File/GetPhoto?id=' + CAST(p.PhotoID AS VARCHAR(100)) + '&width=150" />' + CHAR(13)+CHAR(10)
    END + sl.Description [Note],
    sl.Recorded [DateCreated],
    'Airtest' [NoteType],
    at.AirTestID [ItemId],
    dbo.FormatTeamsReference('J', j.JobNo) +
        CASE WHEN at.AirTestNo > 0
            THEN
                CASE WHEN at.AirTestNo <= 99
                    THEN ' / ' + ISNULL(at.SystemName,'') + RIGHT('0' + CAST(at.AirTestNo AS VARCHAR(100)), 2)
                    ELSE ' / ' + ISNULL(at.SystemName,'') + CAST(at.AirTestNo AS VARCHAR(100))
                END
            ELSE ''
        END
    [ItemNo],
    e.FullName [PortalUser],
    cfg.s__CompanyName [Company],
    c.ClientID [ClientId],
    c.Client [Client]
FROM
    Job j WITH (NOLOCK)
    INNER JOIN Client c WITH (NOLOCK) ON j.ClientID = c.ClientID
    INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
    INNER JOIN AirTest at WITH (NOLOCK) ON je.JobEmployeeID = at.JobEmployeeID
    INNER JOIN SiteLog sl WITH (NOLOCK) ON at.AirTestID = sl.AirTestID
    INNER JOIN Employee e WITH (NOLOCK) ON sl.EmployeeID = e.EmployeeID
    INNER JOIN Photo p WITH (NOLOCK) ON sl.PhotoID = p.PhotoID AND p.ContentType IS NOT NULL
    CROSS JOIN Config cfg WITH (NOLOCK)
WHERE
    cfg.b__ShowAirTestImagesPortalNotes = 1

GO

IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaOutletsWithBlankFlushing')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaOutletsWithBlankFlushing] AS BEGIN SET NOCOUNT ON; END')
END
GO
ALTER PROCEDURE [dbo].[GetPortalLegionellaOutletsWithBlankFlushing]
    @PortalUserID INT = NULL,
    @StartDate DATETIME = NULL,
    @FinishDate DATETIME = NULL,
    @ClientIDs VARCHAR(MAX) = NULL,
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = NULL
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @StartDate = ISNULL(@StartDate, '01/01/2008'),
        @FinishDate = ISNULL(@FinishDate, GETDATE()),
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), '')

    -- Cast DATETIME to DATE. Add one day to the FinishDate - this will set @FinishDate as the next day but it will be at midnight.
    SET @StartDate  = CAST(@StartDate AS DATE)
    SET @FinishDate = DATEADD(d, 1, CAST(@FinishDate AS DATE))

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Sites table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    INSERT INTO @SiteIdData (SiteID)
    SELECT LTRIM(RTRIM(s)) [SiteID]
    FROM dbo.SplitString(@SiteIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get Legionella Outlets data up front to reduce table scans on the Legionella Outlet table.
    DECLARE @LegionellaOutletData TABLE (SiteID INT NOT NULL, JobID INT NOT NULL, LegionellaID INT NOT NULL, LegGUID VARCHAR(MAX), LegGUIDVersion INT, DateApproved DATETIME NOT NULL, LegionellaLocationID INT NOT NULL, Location VARCHAR(MAX) NOT NULL, LocationGUID VARCHAR(MAX), LocationGUIDVersion INT, LegionellaOutletID INT NOT NULL, OutletSystemRef VARCHAR(8000), OutletGUID VARCHAR(MAX), OutletGUIDVersion INT, OutletEnabledCold BIT NOT NULL, OutletEnabledHot BIT NOT NULL, OutletEnabledMixed BIT NOT NULL, OutletEnabledMains BIT NOT NULL, OutletSentinelType INT NOT NULL, OutletLowUse BIT NOT NULL)

    INSERT INTO @LegionellaOutletData (SiteID, JobID, LegionellaID, LegGUID, LegGUIDVersion, DateApproved, LegionellaLocationID, Location, LocationGUID, LocationGUIDVersion, LegionellaOutletID, OutletSystemRef, OutletGUID, OutletGUIDVersion, OutletEnabledCold, OutletEnabledHot, OutletEnabledMixed, OutletEnabledMains, OutletSentinelType, OutletLowUse)
    SELECT
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID [LegGUID],
        l.GUIDVersion [LegGUIDVersion],
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID [LocationGUID],
        ll.GUIDVersion [LocationGUIDVersion],
        lo.LegionellaOutletID,
        lo.SystemRef [OutletSystemRef],
        lo.GUID [OutletGUID],
        lo.GUIDVersion [OutletGUIDVersion],
        lo.EnabledCold [OutletEnabledCold],
        lo.EnabledHot [OutletEnabledHot],
        lo.EnabledMixed [OutletEnabledMixed],
        lo.EnabledMains [OutletEnabledMains],
        CASE
            WHEN lo.SentinelCold = 2 OR lo.SentinelHot = 2 OR lo.SentinelMixed = 2 OR lo.SentinelMains = 2 THEN 2
            WHEN lo.SentinelCold = 1 OR lo.SentinelHot = 1 OR lo.SentinelMixed = 1 OR lo.SentinelMains = 1 THEN 1
            ELSE 0
        END [OutletSentinelType],
        CASE WHEN lo.LowUseCold = 1 OR lo.LowUseHot = 1 OR lo.LowUseMixed = 1 OR lo.LowUseMains = 1
            THEN 1
            ELSE 0
        END [OutletLowUse]
    FROM
        Job j WITH (NOLOCK)
        INNER JOIN @ClientIdData c ON j.ClientID = c.ClientID
        INNER JOIN @SiteIdData si ON j.SiteID = si.SiteID
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Legionella l WITH (NOLOCK) ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
        INNER JOIN LegionellaLocation ll WITH (NOLOCK) ON l.LegionellaID = ll.LegionellaID AND ll.Deleted IS NULL
        INNER JOIN LegionellaOutlet lo WITH (NOLOCK) ON ll.LegionellaLocationID = lo.LegionellaLocationID AND lo.Deleted IS NULL
    GROUP BY
        j.SiteID,
        j.JobID,
        l.LegionellaID,
        l.GUID,
        l.GUIDVersion,
        l.DateApproved,
        ll.LegionellaLocationID,
        ll.Location,
        ll.GUID,
        ll.GUIDVersion,
        lo.LegionellaOutletID,
        lo.SystemRef,
        lo.GUID,
        lo.GUIDVersion,
        lo.EnabledCold,
        lo.EnabledHot,
        lo.EnabledMixed,
        lo.EnabledMains,
        lo.SentinelCold,
        lo.SentinelHot,
        lo.SentinelMixed,
        lo.SentinelMains,
        lo.LowUseCold,
        lo.LowUseHot,
        lo.LowUseMixed,
        lo.LowUseMains


    -- Start the main SELECT.
    SELECT
        *
    FROM
    (
        SELECT
            lo.LegionellaOutletID,
            CASE WHEN NULLIF(lo.Location, '') IS NOT NULL THEN LTRIM(RTRIM(lo.Location)) + ' ' ELSE '' END +
            CASE WHEN NULLIF(lo.OutletSystemRef, '') IS NOT NULL THEN '(' + LTRIM(RTRIM(lo.OutletSystemRef)) + ')' ELSE '' END [OutletDescription],
            CASE WHEN lo.OutletEnabledCold = 0 THEN 'N/A' ELSE NULL END [ColdFlushed],
            CASE WHEN lo.OutletEnabledHot = 0 THEN 'N/A' ELSE NULL END [HotFlushed],
            CASE WHEN lo.OutletEnabledMixed = 0 THEN 'N/A' ELSE NULL END [MixedFlushed],
            CASE WHEN lo.OutletEnabledMains = 0 THEN 'N/A' ELSE NULL END [MainsFlushed],
            NULL [PerformedByThirdParty],
            NULL [Comments],
            NULL [Recorded],
            DATEADD(WEEK, 1, lo.Recorded) [FlushingNextDue]
        FROM
            (
                SELECT
                    *
                FROM
                    (
                        SELECT -- Get each Legionella Outlet record with the max GUID.
                            lo.SiteID,
                            lo.JobID,
                            lo.LegionellaID,
                            lo.LegGUID,
                            lo.LegGUIDVersion,
                            lo.DateApproved,
                            lo.LegionellaLocationID,
                            lo.Location,
                            lo.LocationGUID,
                            lo.LocationGUIDVersion,
                            lo.LegionellaOutletID,
                            lo.OutletSystemRef,
                            lo.OutletGUID,
                            lo.OutletGUIDVersion,
                            lo.OutletEnabledCold,
                            lo.OutletEnabledHot,
                            lo.OutletEnabledMixed,
                            lo.OutletEnabledMains,
                            lo.OutletSentinelType,
                            MAX(locd.Recorded) [Recorded],
                            ROW_NUMBER() OVER (PARTITION BY ISNULL(lo.OutletGUID, NEWID()) ORDER BY lo.OutletGUIDVersion DESC) [RowID]
                        FROM
                            @LegionellaOutletData lo
                            INNER JOIN LegionellaOutletComputedData locd WITH (NOLOCK) ON lo.LegionellaOutletID = locd.LegionellaOutletID
                        WHERE locd.LowUseCold = 1 OR locd.LowUseHot = 1 OR locd.LowUseMixed = 1 OR locd.LowUseMains = 1
                        GROUP BY
                            lo.SiteID,
                            lo.JobID,
                            lo.LegionellaID,
                            lo.LegGUID,
                            lo.LegGUIDVersion,
                            lo.DateApproved,
                            lo.LegionellaLocationID,
                            lo.Location,
                            lo.LocationGUID,
                            lo.LocationGUIDVersion,
                            lo.LegionellaOutletID,
                            lo.OutletSystemRef,
                            lo.OutletGUID,
                            lo.OutletGUIDVersion,
                            lo.OutletEnabledCold,
                            lo.OutletEnabledHot,
                            lo.OutletEnabledMixed,
                            lo.OutletEnabledMains,
                            lo.OutletSentinelType
                    ) lo
                WHERE lo.RowID = 1
            ) lo
    ) a
    WHERE
        (a.FlushingNextDue >= @StartDate AND a.FlushingNextDue < @FinishDate)
    ORDER BY
        a.OutletDescription


    SET NOCOUNT OFF;
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'UnmanagedSiteLeg' AND OBJECT_ID = OBJECT_ID('Site'))
BEGIN
	ALTER TABLE [Site] ADD [UnmanagedSiteLeg] INT NOT NULL
	CONSTRAINT [DF_Site_UnmanagedSiteLeg]  DEFAULT ((0))
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'LegionellaAssetID' AND OBJECT_ID = OBJECT_ID('LegionellaOutletPortalData'))
BEGIN
    ALTER TABLE [dbo].[LegionellaOutletPortalData]
    ADD [LegionellaAssetID] [int] NULL;
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'LegionellaAssetOutletQuestionID' AND OBJECT_ID = OBJECT_ID('LegionellaOutletPortalData'))
BEGIN
    ALTER TABLE [dbo].[LegionellaOutletPortalData]
    ADD [LegionellaAssetOutletQuestionID] [int] NULL;
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'Answer' AND OBJECT_ID = OBJECT_ID('LegionellaOutletPortalData'))
BEGIN
    ALTER TABLE [dbo].[LegionellaOutletPortalData]
    ADD [Answer] [varchar] (MAX) NULL;

    ALTER TABLE LegionellaOutletPortalData ALTER COLUMN LegionellaOutletID INT NULL;
    ALTER TABLE LegionellaOutletPortalData ALTER COLUMN LegionellaLocationID INT NULL
END
GO

IF NOT EXISTS(SELECT * FROM sys.columns WHERE name = 'PortalQuestion' AND OBJECT_ID = OBJECT_ID('LegionellaAssetOutletQuestion'))
BEGIN
    ALTER TABLE [dbo].[LegionellaAssetOutletQuestion]
    ADD [PortalQuestion] [bit] NOT NULL
    CONSTRAINT [DF_LegionellaAssetOutletQuestion_PortalQuestion]
    DEFAULT (0);
END
GO

IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetPortalLegionellaAssetTemps')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetPortalLegionellaAssetTemps] AS BEGIN SET NOCOUNT ON; END')
END
GO

ALTER PROCEDURE [dbo].[GetPortalLegionellaAssetTemps]
	@SiteID INT,
	@LegionellaAssetID INT = 0

AS
BEGIN
	SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
	SET NOCOUNT ON;

-- Default the variable if nothing is passed in
SET @LegionellaAssetID = ISNULL(@LegionellaAssetID, 0)

-- Get Legionella Assets data up front
DECLARE @LegionellaAssetData TABLE (SiteID INT, JobID INT, LegionellaID INT, LegionellaAssetID INT, LegionellaAssetCategoryID INT, SystemRef VARCHAR(MAX), 
									Location VARCHAR(MAX), AssetGUID VARCHAR(MAX), AssetGuidVersion INT, RowID INT)

INSERT INTO @LegionellaAssetData (SiteID, JobID, LegionellaID, LegionellaAssetID, LegionellaAssetCategoryID, SystemRef, Location, AssetGUID, AssetGuidVersion, RowID)
SELECT
    j.SiteID,
    j.JobID,
    l.LegionellaID,
	la.LegionellaAssetID,
	la.LegionellaAssetCategoryID,
	la.SystemRef,
	la.Location,
	la.GUID,
	la.GUIDVersion,
	ROW_NUMBER() OVER (PARTITION BY ISNULL(la.GUID, NEWID()) ORDER BY la.GUIDVersion DESC) [RowID]
FROM
    Job j
    LEFT JOIN Project p ON j.ProjectID = p.ProjectID
    INNER JOIN Quote q ON j.JobID = q.JobID AND q.Rejected IS NULL
    INNER JOIN Appointment a ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
    INNER JOIN JobEmployee je ON j.JobID = je.JobID
    INNER JOIN Legionella l ON je.JobEmployeeID = l.JobEmployeeID AND l.DateApproved IS NOT NULL
    INNER JOIN LegionellaAsset la ON l.LegionellaID = la.LegionellaID AND la.Deleted IS NULL	
WHERE
	j.SiteID = @SiteID

-- Start the main select
IF @LegionellaAssetID = 0
BEGIN
	SELECT
		la.LegionellaID,
		la.LegionellaAssetID,
		laoq.LegionellaAssetOutletQuestionID,
		la.SystemRef,
		la.Location,
		laoq.Description,
		ISNULL(lopd.Answer, laodc.DataText) [DataText],
		'' [PerformedByThirdParty],
		ISNULL(lopd.Recorded, NULL) [Recorded],
		ISNULL(lopd.Comments, '') [Comments]
	FROM
		@LegionellaAssetData la
		INNER JOIN LegionellaAssetOutletQuestion laoq ON la.LegionellaAssetCategoryID = laoq.LegionellaAssetCategoryID AND laoq.PortalQuestion = 1
		LEFT JOIN LegionellaAssetOutletDataCollection laodc ON la.LegionellaAssetID = laodc.LegionellaAssetID AND laoq.LegionellaAssetOutletQuestionID = laodc.LegionellaAssetOutletQuestionID	
		OUTER APPLY
		(
			SELECT TOP 1
				lopd.Answer,
				lopd.Recorded,
				lopd.Comments
			FROM
				LegionellaOutletPortalData lopd 
			WHERE
				la.LegionellaAssetID = lopd.LegionellaAssetID 
					AND 
				laoq.LegionellaAssetOutletQuestionID = lopd.LegionellaAssetOutletQuestionID
			ORDER BY
				lopd.LegionellaOutletPortalDataID DESC
		) lopd
	WHERE
		la.RowID = 1
	ORDER BY
		la.SystemRef
END
ELSE
BEGIN
	SELECT
		la.LegionellaID,
		la.LegionellaAssetID,
		laoq.LegionellaAssetOutletQuestionID,
		la.SystemRef,
		la.Location,
		laoq.Description,
		ISNULL(lopd.Answer, laodc.DataText) [DataText],
		ISNULL(lopd.PerformedByThirdParty, pu.FullName), [PerformedByThirdParty],
		ISNULL(lopd.Recorded, NULL) [Recorded],
		ISNULL(lopd.Comments, '') [Comments]
	FROM
		@LegionellaAssetData la
		LEFT JOIN LegionellaAssetOutletQuestion laoq ON la.LegionellaAssetCategoryID = laoq.LegionellaAssetCategoryID AND laoq.PortalQuestion = 1
		LEFT JOIN LegionellaAssetOutletDataCollection laodc ON la.LegionellaAssetID = laodc.LegionellaAssetID AND laoq.LegionellaAssetOutletQuestionID = laodc.LegionellaAssetOutletQuestionID
		LEFT JOIN LegionellaOutletPortalData lopd ON la.LegionellaAssetID = lopd.LegionellaAssetID AND laoq.LegionellaAssetOutletQuestionID = lopd.LegionellaAssetOutletQuestionID	
		LEFT JOIN PortalUser pu ON lopd.PerformedByPortalUserID = pu.PortalUserId
	WHERE
		la.LegionellaAssetID = @LegionellaAssetID
			AND		
		la.RowID = 1
	ORDER BY
		la.SystemRef
END	


SET NOCOUNT OFF;
END
GO

IF NOT EXISTS (SELECT 1 FROM sys.objects WHERE type = 'P' AND name = 'GetSitesSurveyedLegionella')
BEGIN
    EXEC('CREATE PROCEDURE [dbo].[GetSitesSurveyedLegionella] AS BEGIN SET NOCOUNT ON; END')
END
GO

ALTER PROCEDURE [dbo].[GetSitesSurveyedLegionella]
	@ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = ''	

AS
BEGIN
    SET NOCOUNT ON;
	SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

DECLARE
	@QuestionID INT = (SELECT laoq.LegionellaAssetOutletQuestionID FROM LegionellaAssetOutletQuestion laoq WHERE laoq.ReplaceVariable = '[DateOfNextAssessment]'),
	@Now DATETIME = (SELECT GETDATE())

-- Set default variable values if not passed in.
SELECT
    @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
    @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
    @ProjectID = NULLIF(@ProjectID, 0),
    @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), '')

-- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
INSERT INTO #ClientIdData (ClientID)
SELECT 
	LTRIM(RTRIM(s)) [ClientID]
FROM 
	dbo.SplitString(@ClientIDs, ',')
WHERE 
	NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
GROUP BY 
	s

-- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

IF @SiteIDs IS NOT NULL
BEGIN -- Restriction of Sites.
    INSERT INTO @ClientSiteData (ClientID, SiteID)
    SELECT
        c.ClientID,
        si.SiteID
    FROM
        #ClientIdData c
        INNER JOIN ClientSite cs ON c.ClientID = cs.ClientID
        INNER JOIN Site si ON cs.SiteID = si.SiteID
        INNER JOIN 
		(
            SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
        ) sis ON si.SiteID = sis.SiteID
        LEFT JOIN ProjectSite ps ON si.SiteID = ps.SiteID
        LEFT JOIN Project p ON ps.ProjectID = p.ProjectID
    WHERE
        si.Deleted IS NULL
            AND
        si.InactiveSite = 0
            AND
        ( -- Project Filter.
            (
                @ProjectGroupID IS NULL
                    AND
                @ProjectID IS NULL
            )
                OR
            (
                p.Deleted IS NULL
                    AND
                (
                    p.ProjectGroupID = @ProjectGroupID
                        OR
                    p.ProjectID = @ProjectID
                )
            )
        )
    GROUP BY
        c.ClientID,
        si.SiteID
END
ELSE
BEGIN -- No restriction of Sites.
    INSERT INTO @ClientSiteData (ClientID, SiteID)
    SELECT
        c.ClientID,
        si.SiteID
    FROM
        #ClientIdData c
        INNER JOIN ClientSite cs ON c.ClientID = cs.ClientID
        INNER JOIN Site si ON cs.SiteID = si.SiteID
        LEFT JOIN ProjectSite ps ON si.SiteID = ps.SiteID
        LEFT JOIN Project p ON ps.ProjectID = p.ProjectID
    WHERE
        si.Deleted IS NULL
            AND
        si.InactiveSite = 0
            AND
        ( -- Project Filter.
            (
                @ProjectGroupID IS NULL
                    AND
                @ProjectID IS NULL
            )
                OR
            (
                p.Deleted IS NULL
                    AND
                (
                    p.ProjectGroupID = @ProjectGroupID
                        OR
                    p.ProjectID = @ProjectID
                )
            )
        )
    GROUP BY
        c.ClientID,
        si.SiteID
END

-- Get all Site Jobs up front to reduce table scans (get the most recent job for each Site).
CREATE TABLE #SiteJobs (SiteID INT, UnmanagedSiteLeg BIT, JobID INT, MaxLegionellaFinish DATETIME, DateOfNextAssessment DATETIME)

CREATE INDEX temp_SiteJobs ON #SiteJobs (SiteID)

INSERT INTO #SiteJobs (SiteID, UnmanagedSiteLeg, JobID, MaxLegionellaFinish, DateOfNextAssessment)
SELECT 
	a.SiteID, 
	a.UnmanagedSiteLeg, 
	a.JobID, 
	a.LegionellaFinish [MaxLegionellaFinish],
	CASE
		WHEN NULLIF(a.DateOfNextAssessment, '') IS NULL THEN NULL
	ELSE
		CONVERT(DATETIME, a.DateOfNextAssessment, 103)
	END
FROM
(
    SELECT
        si.SiteID,
        si.UnmanagedSiteLeg,
        jd.JobID,
        jd.LegionellaFinish,
		jd.DateOfNextAssessment,
        ROW_NUMBER() OVER(PARTITION BY si.SiteID ORDER BY jd.LegionellaFinish DESC, jd.JobID DESC) [RowID]
    FROM
        @ClientSiteData csd
        INNER JOIN (
            SELECT
                a.*,
                ROW_NUMBER() OVER(PARTITION BY a.ClientID, a.SiteID ORDER BY a.LegionellaFinish DESC, a.JobID DESC) [RowID]
            FROM
				(
					SELECT 
						j.ClientID, 
						j.SiteID, 
						j.JobID, 
						l.LegionellaFinish,
						laodc.DataText [DateOfNextAssessment]
					FROM
						Job j 
						INNER JOIN Quote q ON j.JobID = q.JobID
						INNER JOIN Appointment a ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
						INNER JOIN JobEmployee je ON j.JobID = je.JobID
						INNER JOIN Legionella l ON je.JobEmployeeID = l.JobEmployeeID
						INNER JOIN LegionellaAssetOutletDataCollection laodc ON l.LegionellaID = laodc.LegionellaID	AND laodc.LegionellaAssetOutletQuestionID = @QuestionID
					WHERE 
						j.Approved IS NOT NULL 
							AND 
						j.Cancelled IS NULL
				) a
        ) jd ON csd.ClientID = ISNULL(jd.ClientID, -1) AND csd.SiteID = jd.SiteID AND jd.RowID = 1
        INNER JOIN Site si ON csd.SiteID = si.SiteID
    GROUP BY
        si.SiteID,
        si.UnmanagedSiteLeg,
        jd.JobID,
        jd.LegionellaFinish,
		jd.DateOfNextAssessment,
        jd.RowID
) a
WHERE 
	a.RowID = 1
GROUP BY
    a.SiteID,
    a.UnmanagedSiteLeg,
    a.JobID,
    a.LegionellaFinish,
	a.DateOfNextAssessment,
    a.RowID
ORDER BY
    a.SiteID

DECLARE 
	@TotalItems INT = (SELECT COUNT(*) FROM #SiteJobs)

-- Start the main select
SELECT
    [category] =	CASE
						WHEN sj.UnmanagedSiteLeg = 1 
						THEN 'Un-managed Site (No assessment required)'
					ELSE
						CASE
							WHEN sj.DateOfNextAssessment < @Now THEN 'Reinspection Overdue'
							WHEN sj.DateOfNextAssessment IS NULL THEN 'Next assessment not specified'
							WHEN sj.DateOfNextAssessment < DATEADD(MONTH, 3, sj.MaxLegionellaFinish) THEN 'Compliant - Reinspection due within 3 months'							
						ELSE 
							'Compliant'
						END
					END,
    [Colour] =		CASE
						WHEN sj.UnmanagedSiteLeg = 1 THEN '#CCCCCC'
					ELSE
						CASE
							WHEN sj.DateOfNextAssessment < @Now THEN '#E8412D'
							WHEN sj.DateOfNextAssessment IS NULL THEN '#00B050'
							WHEN sj.DateOfNextAssessment < DATEADD(MONTH, 3, sj.MaxLegionellaFinish) THEN '#F6BD0F'							
						ELSE 
							'#AFD8F8'
						END
					END,
    [Share] =		COUNT(*),
    [TotalItems] =	@TotalItems    
FROM
	#SiteJobs sj
GROUP BY
    CASE
		WHEN sj.UnmanagedSiteLeg = 1 
		THEN 'Un-managed Site (No assessment required)'
	ELSE
		CASE
			WHEN sj.DateOfNextAssessment < @Now THEN 'Reinspection Overdue'
			WHEN sj.DateOfNextAssessment IS NULL THEN 'Next assessment not specified'
			WHEN sj.DateOfNextAssessment < DATEADD(MONTH, 3, sj.MaxLegionellaFinish) THEN 'Compliant - Reinspection due within 3 months'							
		ELSE 
			'Compliant'
		END
	END,
	CASE
		WHEN sj.UnmanagedSiteLeg = 1 THEN '#CCCCCC'
	ELSE
		CASE
			WHEN sj.DateOfNextAssessment < @Now THEN '#E8412D'
			WHEN sj.DateOfNextAssessment IS NULL THEN '#00B050'
			WHEN sj.DateOfNextAssessment < DATEADD(MONTH, 3, sj.MaxLegionellaFinish) THEN '#F6BD0F'							
		ELSE 
			'#AFD8F8'
		END
	END
ORDER BY    
    category

-- Clear up temp tables.
DROP TABLE #ClientIdData
DROP TABLE #SiteJobs

    SET NOCOUNT OFF;
END



GO

ALTER PROCEDURE [dbo].[GetAirTests]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @ClientOrderNo VARCHAR(50) = '',
    @UPRN VARCHAR(50) = '',
    @AddressSearchString VARCHAR(200) = '',
    @JobNo INT = 0,
    @JobNoSearch INT = 0,
    @GetSiteDocuments INT = 0, /* 0 or NULL = Any, 1 = Yes, 2 = No */
	@AirTestTypeID INT = 0
/**********************************************************************
** Overview: Get a filtered collection of air tests.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @ClientOrderNo = NULLIF(LTRIM(RTRIM(@ClientOrderNo)), ''),
        @UPRN = NULLIF(LTRIM(RTRIM(@UPRN)), ''),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), ''),
        @JobNo = NULLIF(@JobNo, 0),
        @JobNoSearch = NULLIF(@JobNoSearch, 0),
        @GetSiteDocuments = ISNULL(@GetSiteDocuments, 0),	
		@AirTestTypeID = NULLIF(@AirTestTypeID, 0)	

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocClientOrderNo VARCHAR(50) = @ClientOrderNo,
        @LocUPRN VARCHAR(50) = @UPRN,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString,
        @LocJobNo INT = @JobNo,
        @LocJobNoSearch INT = @JobNoSearch,
        @LocGetSiteDocuments INT = @GetSiteDocuments,
		@LocAirTestType INT = @AirTestTypeID

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @b__onlyshowApprovedAirTestsOnPortal BIT
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @b__onlyshowApprovedAirTestsOnPortal = cfg.b__onlyshowApprovedAirTestsOnPortal
    FROM
        Config cfg WITH (NOLOCK)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END

    -- Get all Air Test Data up front to reduce the main SELECT table scans.
    DECLARE @AirTestData TABLE (IsSiteDocument BIT NOT NULL, JobID INT, JobNo INT, ClientOrderNo VARCHAR(50), Created DATETIME NOT NULL, LastNoteCreated DATETIME, ClientID INT NOT NULL, Client VARCHAR(100) NOT NULL, BranchName VARCHAR(100), SiteID INT NOT NULL, Address VARCHAR(200) NOT NULL, Postcode VARCHAR(10) NOT NULL, UPRN VARCHAR(50), AirTestID INT, AirTestNo INT, AirTestStart DATETIME, AirTestFinish DATETIME, SiteArrival DATETIME, LocationEnclosure VARCHAR(MAX), Status VARCHAR(100), SystemName VARCHAR(2), Approved DATETIME, AirTestTypeID INT, AirTestType VARCHAR(50), AirTestReport VARCHAR(100), SiteDocumentID INT, SiteDocumentsCount INT NOT NULL)

    -- Get normal Air Tests first.
    IF @LocGetSiteDocuments <> 1
    BEGIN
        INSERT INTO @AirTestData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, ClientID, Client, BranchName, SiteID, Address, Postcode, UPRN, AirTestID, AirTestNo, AirTestStart, AirTestFinish, SiteArrival, LocationEnclosure, Status, SystemName, Approved, AirTestTypeID, AirTestType, AirTestReport, SiteDocumentID, SiteDocumentsCount)
        SELECT
            0 [IsSiteDocument],
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            MAX(n.DateCreated) [LastNoteCreated],
            c.ClientID,
            c.Client,
            c.BranchName,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            at.AirTestID,
            at.AirTestNo,
            at.AirTestStart,
            at.AirTestFinish,
            at.SiteArrival,
            at.LocationEnclosure,
            at.Status,
            at.SystemName,
            at.OfficeApprovedDate [Approved],
            att.AirTestTypeID,
            att.Description [AirTestType],
            pdfFile.FileName [AirTestReport],
            NULL [SiteDocumentID],
            0 [SiteDocumentsCount]
        FROM
            Job j WITH (NOLOCK)
            INNER JOIN Client c WITH (NOLOCK) ON j.ClientID = c.ClientID AND c.Deleted IS NULL
            INNER JOIN #ClientIdData cid ON c.ClientID = cid.ClientID
            LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
            INNER JOIN Site si WITH (NOLOCK) ON j.SiteID = si.SiteID
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
            INNER JOIN AirTest at WITH (NOLOCK) ON je.JobEmployeeID = at.JobEmployeeID AND CASE WHEN @b__onlyshowApprovedAirTestsOnPortal = 1 THEN CASE WHEN at.OfficeApprovedDate IS NOT NULL THEN 1 ELSE 0 END ELSE 1 END = 1
            INNER JOIN AirTestType att WITH (NOLOCK) ON at.AirTestTypeID = att.AirTestTypeID
            LEFT JOIN Note n WITH (NOLOCK) ON at.AirTestID = n.ItemID AND n.NoteTypeID = 5 AND n.PortalUserID IS NOT NULL
            OUTER APPLY
            (
                SELECT TOP 1 _pf.FileName [FileName]
                FROM PDF _pf WITH (NOLOCK)
                WHERE
                    _pf.JobID = j.JobID
                        AND
                    _pf.DateDeleted IS NULL
                        AND
                    _pf.FileName NOT LIKE '%ra%'
                        AND
                    _pf.FileName LIKE '%\_' + CAST(at.AirTestID AS VARCHAR) + ' (%' ESCAPE '\'
                ORDER BY
                    _pf.DateCreated DESC
            ) pdfFile
        WHERE
            si.Deleted IS NULL
                AND
            j.Cancelled IS NULL
                AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocClientOrderNo IS NULL -- Client Order No Filter.
                THEN 1
                ELSE CASE WHEN j.ClientOrderNo = @LocClientOrderNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocUPRN IS NULL -- UPRN Filter.
                THEN 1
                ELSE CASE WHEN si.UPRN = @LocUPRN THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
                THEN 1
                ELSE
                    CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR j.ClientOrderNo = @LocAddressSearchString
                        THEN 1
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocJobNo IS NULL -- Job Filter.
                THEN 1
                ELSE CASE WHEN j.JobNo = @LocJobNo THEN 1 ELSE 0 END
            END = 1
                AND
            CASE WHEN @LocJobNoSearch IS NULL -- Job Search Filter.
                THEN 1
                ELSE CASE WHEN CAST(j.JobNo AS VARCHAR(50)) LIKE CAST(@LocJobNoSearch AS VARCHAR(50)) + '%' THEN 1 ELSE 0 END
            END = 1
				AND
			CASE 
				WHEN @AirTestTypeID IS NULL
				THEN 1
			ELSE
				CASE WHEN at.AirTestTypeID = @AirTestTypeID THEN 1 ELSE 0 END
			END = 1
        GROUP BY
            j.JobID,
            j.JobNo,
            j.ClientOrderNo,
            j.Created,
            c.ClientID,
            c.Client,
            c.BranchName,
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            at.AirTestID,
            at.AirTestNo,
            at.AirTestStart,
            at.AirTestFinish,
            at.SiteArrival,
            at.LocationEnclosure,
            at.Status,
            at.SystemName,
            at.OfficeApprovedDate,
            att.AirTestTypeID,
            att.Description,
            pdfFile.FileName
        ORDER BY
            ISNULL(at.OfficeApprovedDate, at.AirTestStart) DESC,
            j.JobNo DESC,
            at.AirTestNo DESC
    END

    -- Get the Site Documents.
    IF @LocGetSiteDocuments <> 2
    BEGIN
        INSERT INTO @AirTestData (IsSiteDocument, JobID, JobNo, ClientOrderNo, Created, LastNoteCreated, ClientID, Client, BranchName, SiteID, Address, Postcode, UPRN, AirTestID, AirTestNo, AirTestStart, AirTestFinish, SiteArrival, LocationEnclosure, Status, SystemName, Approved, AirTestTypeID, AirTestType, AirTestReport, SiteDocumentID, SiteDocumentsCount)
        SELECT
            1 [IsSiteDocument],
            NULL [JobID],
            NULL [JobNo],
            NULL [ClientOrderNo],
            sidoc.Uploaded [Created],
            NULL [LastNoteCreated],
            MAX(c.ClientID) [ClientID],
            MAX(c.Client) [Client],
            MAX(c.BranchName) [BranchName],
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
            NULL [AirTestID],
            NULL [AirTestNo],
            sidoci.WorkDate [AirTestStart],
            sidoci.WorkDate [AirTestFinish],
            NULL [SiteArrival],
            NULL [LocationEnclosure],
            NULL [Status],
            NULL [SystemName],
            NULL [Approved],
            NULL [AirTestTypeID],
            NULL [AirTestType],
            sidoc.FileName [AirTestReport],
			sidoc.SiteDocumentId,
            1 [SiteDocumentsCount]
        FROM
            Site si WITH (NOLOCK)
            LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
            INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
            INNER JOIN Client c WITH (NOLOCK) ON cs.ClientID = c.ClientID
            INNER JOIN #ClientIdData cid ON c.ClientID = cid.ClientID

			INNER JOIN SiteDocument sidoc WITH (NOLOCK) ON si.SiteID = sidoc.SiteID
			INNER JOIN SiteDocumentInformation sidoci WITH (NOLOCK) ON sidoc.SiteDocumentId = sidoci.SiteDocumentID

            --OUTER APPLY
            --(
            --    SELECT TOP 1
            --        sid.SiteDocumentID,
            --        sid.SiteID,
            --        sid.EmployeeID,
            --        sid.PortalUserID,
            --        sid.FileName,
            --        sid.Uploaded,
            --        sidi.SiteDocumentInformationID,
            --        sidi.WorkDate
            --    FROM
            --        SiteDocument sid WITH (NOLOCK)
            --        INNER JOIN SiteDocumentInformation sidi WITH (NOLOCK) ON sid.SiteDocumentID = sidi.SiteDocumentID
            --    WHERE
            --        sid.SiteID = si.SiteID
            --            AND
            --        sid.Deleted IS NULL
            --            AND
            --        sid.SiteDocumentTypeID = 5
            --    ORDER BY
            --        sid.Uploaded DESC
            --) sid
            --OUTER APPLY
            --(
            --    SELECT COUNT(*) [SiteDocumentCount]
            --    FROM SiteDocument sid WITH (NOLOCK)
            --    WHERE
            --        sid.SiteID = si.SiteID
            --            AND
            --        sid.Deleted IS NULL
            --            AND
            --        sid.SiteDocumentTypeID = 5
            --) sidc
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            --sid.SiteDocumentID IS NOT NULL
            --    AND
            CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
                THEN 1
                ELSE
                    CASE WHEN p.Deleted IS NULL
                        THEN CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
                        ELSE 0
                    END
            END = 1
                AND
            CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
                THEN 1
                ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
            END = 1
				AND
			sidoc.Deleted IS NULL
				AND
			sidoc.SiteDocumentTypeID = 5
        GROUP BY
            si.SiteID,
            si.Address,
            si.Postcode,
            si.UPRN,
			sidoc.SiteDocumentId,
            sidoc.FileName,
            sidoc.Uploaded,
            sidoci.WorkDate--,
            --sidc.SiteDocumentCount
        ORDER BY
            sidoc.Uploaded DESC
    END
    -- Start the main SELECT
    SELECT *
    FROM @AirTestData
    ORDER BY
        ISNULL(Approved, Created) DESC,
        JobNo DESC,
        AirTestNo DESC,
        IsSiteDocument

    -- Clear up temp tables.
    DROP TABLE #ClientIdData

    SET NOCOUNT OFF;
END
GO

USE [TEAMS]
GO

/****** Object:  StoredProcedure [dbo].[GetPortalHomeTabGraphData]    Script Date: 04/19/2018 14:57:12 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO


ALTER PROCEDURE [dbo].[GetPortalHomeTabGraphData]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @JobID INT = NULL,
    @ReturnAsChart BIT = NULL
/**********************************************************************
** Overview: Get data which is used by the graphs on the Home tab of the Portal. Currently just Risk and Recommended Action items.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), ''),
        @JobID = NULLIF(@JobID, 0),
        @ReturnAsChart = ISNULL(@ReturnAsChart, 0)

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT PRIMARY KEY)
    INSERT INTO @ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@ClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    DECLARE @SurveyTypeIdData TABLE (SurveyTypeID INT PRIMARY KEY)
    INSERT INTO @SurveyTypeIdData (SurveyTypeID)
    SELECT s
    FROM dbo.SplitString(@SurveyTypeIDs, ',')

    -- Get the assigned Client and Site data up front to reduce table scans on the Client/Site table.
    DECLARE @ClientSiteData TABLE (ClientID INT, SiteID INT)

    IF @SiteIDs IS NOT NULL
    BEGIN -- Restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            INNER JOIN (
                SELECT LTRIM(RTRIM(s)) [SiteID] FROM dbo.SplitString(@SiteIDs, ',') WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL GROUP BY s
            ) sis ON si.SiteID = sis.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END
    ELSE
    BEGIN -- No restriction of Sites.
        INSERT INTO @ClientSiteData (ClientID, SiteID)
        SELECT
            c.ClientID,
            si.SiteID
        FROM
            @ClientIdData c
            INNER JOIN ClientSite cs WITH (NOLOCK) ON c.ClientID = cs.ClientID
            INNER JOIN Site si WITH (NOLOCK) ON cs.SiteID = si.SiteID
            LEFT JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
            LEFT JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        WHERE
            si.Deleted IS NULL
                AND
            si.InactiveSite = 0
                AND
            si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this graph. */
                AND
            ( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
        GROUP BY
            c.ClientID,
            si.SiteID
    END

    -- Get Register data up front to reduce table scans.
    CREATE TABLE #RegisterData (ClientID INT, SiteID INT, JobID INT, JobNo INT, RegisterID INT, BuildingDesignation VARCHAR(1000), RegisterFinish DATETIME, SurveyTypeID INT)

    -- Add an index on important #RegisterData fields to increase speed below.
    CREATE INDEX temp_RegisterData ON #RegisterData (RegisterID)

    INSERT INTO #RegisterData (ClientID, SiteID, JobID, JobNo, RegisterID, BuildingDesignation, RegisterFinish, SurveyTypeID)
    SELECT
        csd.ClientID,
        csd.SiteID,
        j.JobID,
        j.JobNo,
        r.RegisterID,
        r.BuildingDesignation,
        r.RegisterFinish,
        su.SurveyTypeID
    FROM
        @ClientSiteData csd
        INNER JOIN Job j WITH (NOLOCK) ON csd.ClientID = j.ClientID AND csd.SiteID = j.SiteID AND j.Cancelled IS NULL AND j.Approved IS NOT NULL
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        INNER JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN @SurveyTypeIdData sut ON su.SurveyTypeID = sut.SurveyTypeID
        LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID
    WHERE
        (@JobID IS NULL OR j.JobID = @JobID)
			AND
		( -- Project Filter.
                (
                    @ProjectGroupID IS NULL
                        AND
                    @ProjectID IS NULL
                )
                    OR
                (
                    p.Deleted IS NULL
                        AND
                    (
                        p.ProjectGroupID = @ProjectGroupID
                            OR
                        p.ProjectID = @ProjectID
                    )
                )
            )
    GROUP BY
        csd.ClientID,
        csd.SiteID,
        j.JobID,
        j.JobNo,
        r.RegisterID,
        r.BuildingDesignation,
        r.RegisterFinish,
        su.SurveyTypeID

    -- Get SampleComputedData data up front to reduce table scans on the SampleComputedData table.
    DECLARE @SiteSampleComputedData TABLE (SampleComputedDataID INT, RegisterID INT, SampleID INT, SampleResult INT, RecommendedAction NVARCHAR(100), RecommendedActionColour VARCHAR(6), RecommendedActionSortOrder INT, RiskScore INT, RiskScoreGroupID INT, RiskScoreGroup VARCHAR(100), RiskScoreGroupColour VARCHAR(10), RiskScoreSortOrder INT)

    INSERT INTO @SiteSampleComputedData (SampleComputedDataID, RegisterID, SampleID, SampleResult, RecommendedAction, RecommendedActionColour, RecommendedActionSortOrder, RiskScore, RiskScoreGroupID, RiskScoreGroup, RiskScoreGroupColour, RiskScoreSortOrder)
    SELECT
        scd.SampleComputedDataID,
        r.RegisterID,
        scd.SampleID,
        scd.SampleResult,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        scd.RecommendedActionSortOrder,
        scd.RiskScore,
        scd.RiskScoreGroupID,
        scd.RiskScoreGroup,
        scd.RiskScoreGroupColour,
        scd.RiskScoreSortOrder
    FROM
        #RegisterData r
        INNER JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
        INNER JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        INNER JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID
        INNER JOIN GuidSamples gs WITH (NOLOCK) ON s.SampleID = gs.SampleID AND gs.ClientId=r.ClientID AND gs.SiteId=r.SiteID
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON gs.SampleID = scd.SampleID --AND gs.ClientID = scd.ClientID AND gs.SiteID = scd.SiteID
    WHERE
        scd.Removed = 0
            AND
        scd.RecommendedAction IS NOT NULL
			

    -- Clear up temp tables.
    DROP TABLE #RegisterData

    -- Get all Risk Items data up front to reduce table scans.
    DECLARE @RiskItemsData TABLE (SampleID INT NOT NULL, RiskScoreGroupID INT NOT NULL, RiskScoreGroup VARCHAR(100) NOT NULL, RiskScoreGroupColour VARCHAR(10) NOT NULL, RiskScoreSortOrder INT NOT NULL)
    INSERT INTO @RiskItemsData (SampleID, RiskScoreGroupID, RiskScoreGroup, RiskScoreGroupColour, RiskScoreSortOrder)
    SELECT
        SampleID,
        ISNULL(RiskScoreGroupID, -1) [RiskScoreGroupID],
        ISNULL(RiskScoreGroup, 'Inaccessible') [RiskScoreGroup],
        ISNULL(RiskScoreGroupColour, '#CCCCCC') [RiskScoreGroupColour],
        ISNULL(RiskScoreSortOrder, 0) [RiskScoreSortOrder]
    FROM @SiteSampleComputedData
    WHERE SampleResult <> 0
    GROUP BY
        SampleID,
        ISNULL(RiskScoreGroupID, -1),
        ISNULL(RiskScoreGroup, 'Inaccessible'),
        ISNULL(RiskScoreGroupColour, '#CCCCCC'),
        ISNULL(RiskScoreSortOrder, 0)

    -- Get all Recommended Action Items data up front to reduce table scans.
    DECLARE @RecommendedActionItemsData TABLE (RegisterID INT NOT NULL, SampleID INT NOT NULL, RecommendedAction NVARCHAR(100) NOT NULL, RecommendedActionColour VARCHAR(6) NOT NULL, RecommendedActionSortOrder INT NOT NULL)
    INSERT INTO @RecommendedActionItemsData (RegisterID, SampleID, RecommendedAction, RecommendedActionColour, RecommendedActionSortOrder)
    SELECT
        RegisterID,
        SampleID,
        RecommendedAction,
        RecommendedActionColour,
        ISNULL(RecommendedActionSortOrder, 0) [RecommendedActionSortOrder]
    FROM @SiteSampleComputedData
    GROUP BY
        RegisterID,
        SampleID,
        RecommendedAction,
        RecommendedActionColour,
        ISNULL(RecommendedActionSortOrder, 0)

    -- Get the total number of items.
    DECLARE @TotalRiskItems INT = (SELECT COUNT(*) FROM @RiskItemsData)
    DECLARE @TotalRecActionItems INT = (SELECT COUNT(*) FROM @RecommendedActionItemsData)

    -- Start the main SELECTs.
    IF @ReturnAsChart = 1
    BEGIN
        -- Get the Max Sort Order, so we can re-arrange the items.
        DECLARE @MaxRisksSortOrder INT = (SELECT MAX(SortOrder) FROM RiskScoreGroup WITH (NOLOCK))
        DECLARE @RecommendedActionsAllowed INT = (SELECT mc.MobileConfigInt FROM MobileConfig mc WITH (NOLOCK) INNER JOIN MobileConfigType mct WITH (NOLOCK) ON mc.MobileConfigTypeID = mct.MobileConfigTypeID WHERE mct.ConfigType = 'RecommendedActionAllowed')

        -- Start the SELECT to get Risk Items data.
        SELECT
            rsg.RiskScoreGroup [category],
            rsg.RiskScoreGroupColour [Colour],
            COUNT(rid.SampleID) [Share],
            @TotalRiskItems [TotalItems],
            CAST(1 AS BIT) [VisibleInLegend],
            @MaxRisksSortOrder - MAX(rsg.RiskScoreSortOrder) + 1 [SortOrder]
        FROM
            (
                SELECT RiskScoreGroupID, ScoreGroup [RiskScoreGroup], Colour [RiskScoreGroupColour], SortOrder [RiskScoreSortOrder] FROM RiskScoreGroup WITH (NOLOCK)
                UNION ALL
                SELECT -1 [RiskScoreGroupID], 'Inaccessible' [RiskScoreGroup], '#CCCCCC' [RiskScoreGroupColour], CAST(-1 AS INT) [RiskScoreSortOrder]
            ) rsg
            LEFT JOIN @RiskItemsData rid ON rsg.RiskScoreGroupID = ISNULL(rid.RiskScoreGroupID, -1)
        GROUP BY
            rsg.RiskScoreGroup,
            rsg.RiskScoreGroupColour
        ORDER BY
            SortOrder,
            category

        -- Start the SELECT to get Recommended Action Items data.
        SELECT
            ra.RecommendedAction [category],
            '#' + ra.RecommendedActionColour [Colour],
            COUNT(raid.SampleID) [Share],
            @TotalRecActionItems [TotalItems],
            CAST(0 AS BIT) [VisibleInLegend],
            MAX(ra.RecommendedActionSortOrder) [SortOrder]
        FROM
            (
                SELECT
                    ra.RecommendedAction,
                    ra.RecommendedActionColour,
                    CASE WHEN ra.RecommendedAction = 'No further action required'
                        THEN 0
                        ELSE ra.RecommendedActionSortOrder + 1
                    END [RecommendedActionSortOrder]
                FROM
                (
                    SELECT
                        e20.ShortDescription [RecommendedAction],
                        rac.RecommendedActionColour,
						CASE WHEN e20.ShortDescription = 'No further action required'
                        THEN 0
                        ELSE @RecommendedActionsAllowed - e20.ElementIntValue 
					END[RecommendedActionSortOrder]

                    FROM
                        ElementIntMeaning e20 WITH (NOLOCK)
                        INNER JOIN RecommendedActionColour rac WITH (NOLOCK) ON e20.ElementIntValue = rac.ElementIntValue
                    WHERE e20.ElementTypeID = 20
                    UNION
                    SELECT
                        'No further action required' [RecommendedAction],
                        '92A0F4' [RecommendedActionColour],
                        0 [RecommendedActionSortOrder]
                ) ra
                WHERE ra.RecommendedActionSortOrder >= 0
            ) ra
            LEFT JOIN @RecommendedActionItemsData raid ON ra.RecommendedAction = raid.RecommendedAction
        GROUP BY
            ra.RecommendedAction,
            ra.RecommendedActionColour
        ORDER BY
            category,
            SortOrder
    END
    ELSE
    BEGIN
        -- Start the SELECT to get Risk Items data.
        SELECT
            SampleID,
            RiskScoreGroup,
            RiskScoreGroupColour,
            RiskScoreSortOrder
        FROM @RiskItemsData
        GROUP BY
            SampleID,
            RiskScoreGroup,
            RiskScoreGroupColour,
            RiskScoreSortOrder

        -- Start the SELECT to get Recommended Action Items data.
        SELECT
            RegisterID,
            SampleID,
            RecommendedAction,
            RecommendedActionColour,
            ISNULL(RecommendedActionSortOrder, 0) [RecommendedActionSortOrder]
        FROM @RecommendedActionItemsData
        GROUP BY
            RegisterID,
            SampleID,
            RecommendedAction,
            RecommendedActionColour,
            ISNULL(RecommendedActionSortOrder, 0)
    END


    SET NOCOUNT OFF;
END


GO


USE [TEAMS]
GO

/****** Object:  StoredProcedure [dbo].[GetSamplesByRiskRecAction]    Script Date: 04/19/2018 14:59:13 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER OFF
GO



ALTER PROCEDURE [dbo].[GetSamplesByRiskRecAction]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @Risk VARCHAR(100) = '',
    @RecAction VARCHAR(100) = '',
    @SurveyTypeIDs VARCHAR(MAX) = '',
    @MaxNumberOfRows INT = 0
/**********************************************************************
** Overview: Get all Samples for a Portal User based on the Risk or Recommended Action.
** If these aren't passed in, it will get all items with a Recommended Action, used on the Sites tab.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @Risk = NULLIF(LTRIM(RTRIM(@Risk)), ''),
        @RecAction = NULLIF(LTRIM(RTRIM(@RecAction)), ''),
        @SurveyTypeIDs = NULLIF(LTRIM(RTRIM(@SurveyTypeIDs)), '')

    -- Get all Clients up front to reduce table scans on the Clients table and only get the ones needed.
    DECLARE @ClientIdData TABLE (ClientID INT)
    IF @ClientIDs IS NOT NULL
        BEGIN
            INSERT INTO @ClientIdData (ClientID)
            SELECT s
            FROM dbo.SplitString(@ClientIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO @ClientIdData (ClientID)
            SELECT ClientID
            FROM Client WITH (NOLOCK)
            WHERE Deleted IS NULL
        END

    -- Get all Survey Types up front to reduce table scans on the SurveyType table and only get the ones needed.
    DECLARE @SurveyTypeIdData TABLE (SurveyTypeID INT)
    IF @SurveyTypeIDs IS NOT NULL
        BEGIN
            INSERT INTO @SurveyTypeIdData (SurveyTypeID)
            SELECT s
            FROM dbo.SplitString(@SurveyTypeIDs, ',')
        END
    ELSE
        BEGIN
            INSERT INTO @SurveyTypeIdData (SurveyTypeID)
            SELECT SurveyTypeID
            FROM SurveyType WITH (NOLOCK)
            WHERE Deleted IS NULL
        END

    -- Convert @ClientIdData and @SurveyTypeIdData into a comma separated string of IDs. Reset the variables initially passed in.
    SELECT @ClientIDs = '', @SurveyTypeIDs = ''
    SELECT
        @ClientIDs = STUFF((
            SELECT ',' + CAST(ClientID AS VARCHAR(20))
            FROM @ClientIdData
            FOR XML PATH('')), 1, 1, ''),
        @SurveyTypeIDs = STUFF((
            SELECT ',' + CAST(SurveyTypeID AS VARCHAR(20))
            FROM @SurveyTypeIdData
            FOR XML PATH('')), 1, 1, '')

    -- Set variables for logic.
    DECLARE @CompanyName NVARCHAR(50), @BasementName VARCHAR(50)
    SELECT
        @CompanyName = cfg.s__CompanyName,
        @BasementName = ISNULL(NULLIF(cfg.s__BasementName, ''), 'Z-Sub Level')
    FROM
        Config cfg WITH (NOLOCK)

    -- Declare a variable for storing dynamic SQL.
    DECLARE @DynamicSQL NVARCHAR(MAX) = ''

    -- Build the main dynamic query.
    SELECT @DynamicSQL = @DynamicSQL + 'SELECT DISTINCT' + CASE WHEN @MaxNumberOfRows > 0 THEN ' TOP ' + CAST(@MaxNumberOfRows AS VARCHAR(100)) ELSE '' END + '
        j.JobNo,
        si.Address,
        si.Postcode,
        si.UPRN,
        ISNULL(r.BuildingDesignation, ''No Building Designation'') [Building],
        f.FloorNumber,
        ISNULL(f.DescriptionOverride, REPLACE(dbo.FloorName(f.FloorNumber), ''Z-Sub Level'', ''' + @BasementName + ''')) [FloorDescription],
        rm.RoomCode,
        rm.Number [RoomNumber],
        rm.Description [RoomDescription],
        s.RegisterItemNo,
        s.AsSample,
        s.SampleRef,
        scd.SourceDescription,
        scd.AsbestosType,
        scd.MaterialAssessmentScore,
        scd.PriorityAssessmentScore,
        scd.RiskScore,
        scd.RiskScoreGroupColour,
        scd.RecommendedAction,
        scd.RecommendedActionColour,
        DATENAME(MONTH, scd.TimescaleForCompletion) + '' '' + CAST(DATEPART(YEAR, scd.TimescaleForCompletion) AS VARCHAR(4)) [TimescaleForCompletion],
        su.SurveyTypeID,
        j.JobID,
        s.SampleID,
        scd.IsMAOnly,
        CAST(r.RegisterStart AS DATE) [SurveyStartDate],
        CAST(r.RegisterFinish AS DATE) [SurveyFinishDate]
    FROM
        GuidSamples gs WITH (NOLOCK) 
        INNER JOIN SampleComputedData scd WITH (NOLOCK) ON gs.SampleID = scd.SampleID AND gs.ClientID = scd.ClientID AND gs.SiteID = scd.SiteID
        INNER JOIN Sample s WITH (NOLOCK) ON scd.SampleID = s.SampleID
        INNER JOIN Room rm WITH (NOLOCK) ON s.RoomID = rm.RoomID
        INNER JOIN Floorplan f WITH (NOLOCK) ON rm.FloorplanID = f.FloorplanID
        INNER JOIN Register r WITH (NOLOCK) ON f.RegisterID = r.RegisterID AND r.DateApproved IS NOT NULL
        INNER JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        INNER JOIN JobEmployee je WITH (NOLOCK) ON r.JobEmployeeID = je.JobEmployeeID
        INNER JOIN Job j WITH (NOLOCK) ON je.JobID = j.JobID
        INNER JOIN Quote q WITH (NOLOCK) ON j.JobID = q.JobID AND q.Rejected IS NULL
        INNER JOIN Appointment a WITH (NOLOCK) ON q.QuoteID = a.QuoteID AND a.DateDeclined IS NULL
        INNER JOIN Site si WITH (NOLOCK) ON scd.SiteID = si.SiteID
        INNER JOIN Client c WITH (NOLOCK) ON scd.ClientID = c.ClientID
        ' + CASE WHEN @ProjectGroupID IS NOT NULL OR @ProjectID IS NOT NULL THEN '
            LEFT JOIN Project p WITH (NOLOCK) ON j.ProjectID = p.ProjectID'
                ELSE ''
            END + '
    WHERE
        si.Deleted IS NULL
            AND
        si.InactiveSite = 0
            AND
        si.Post2000 = 0 AND si.UnmanagedSite = 0 /* Also hide Post 2000 and Unmanaged Sites for this table. */
            AND
        c.ClientID IN (' + @ClientIDs + ')
            AND
        su.SurveyTypeID IN (' + @SurveyTypeIDs + ')
            AND
        scd.Removed = 0
            AND
        scd.RecommendedAction IS NOT NULL
    '

    -- Add dynamic WHERE filters
    IF @ProjectGroupID IS NOT NULL OR @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.Deleted IS NULL'
    END
    IF @ProjectGroupID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectGroupID = ' + CAST(@ProjectGroupID AS VARCHAR(20))
    END
    IF @ProjectID IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND p.ProjectID = ' + CAST(@ProjectID AS VARCHAR(20))
    END
    IF @SiteIDs IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND si.SiteID IN (' + @SiteIDs + ')'
    END
    IF @Risk = 'Inaccessible'
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND scd.SampleResult <> 0'
    END
    IF @Risk IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND ISNULL(scd.RiskScoreGroup, ''Inaccessible'') = ''' + @Risk + ''''
    END
    IF @RecAction IS NOT NULL
    BEGIN
        SELECT @DynamicSQL = @DynamicSQL + ' AND scd.RecommendedAction = ''' + @RecAction + ''''
    END

    -- Add ORDER BY
    SELECT @DynamicSQL = @DynamicSQL + '
    ORDER BY
        j.JobNo,
        si.Address,
        si.Postcode,
        si.UPRN,
        Building,
        f.FloorNumber,
        rm.RoomCode,
        rm.Number,
        s.RegisterItemNo
    '

    -- Execute the SQL
    EXECUTE sp_executesql @DynamicSQL
    SELECT @DynamicSQL = ''


    SET NOCOUNT OFF;
END



GO




USE [TEAMS]
GO
/****** Object:  StoredProcedure [dbo].[GetProjects]    Script Date: 10/05/2018 09:01:11 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO



ALTER PROCEDURE [dbo].[GetProjects]
    @PortalUserID INT,
    @ClientIDs VARCHAR(MAX) = '',
    @ProjectGroupID INT = NULL,
    @ProjectID INT = NULL,
    @SiteIDs VARCHAR(MAX) = '',
    @Status INT, /* 0 OR NULL = Any, 1 = Approved, 2 = SampleAnalysisComplete, 3 = SiteWorkComplete, 4 = WorkScheduled, 5 = UnScheduled */
    @AddressSearchString VARCHAR(200) = ''
/**********************************************************************
** Overview: Get a filtered collection of Projects.
**
** PLEASE NOTE: Please use SVN as a Change Log.
**********************************************************************/
WITH RECOMPILE
AS
BEGIN
    SET NOCOUNT ON;


    -- Set default variable values if not passed in.
    SELECT
        @ClientIDs = NULLIF(LTRIM(RTRIM(@ClientIDs)), ''),
        @ProjectGroupID = NULLIF(@ProjectGroupID, 0),
        @ProjectID = NULLIF(@ProjectID, 0),
        @SiteIDs = NULLIF(LTRIM(RTRIM(@SiteIDs)), ''),
        @Status = NULLIF(@Status, 0),
        @AddressSearchString = NULLIF(LTRIM(RTRIM(@AddressSearchString)), '')

    -- Setup duplicate local variables due to parameter sniffing problems.
    DECLARE
        @LocPortalUserID INT = @PortalUserID,
        @LocClientIDs VARCHAR(MAX) = @ClientIDs,
        @LocProjectGroupID INT = @ProjectGroupID,
        @LocProjectID INT = @ProjectID,
        @LocSiteIDs VARCHAR(MAX) = @SiteIDs,
        @LocStatus INT = @Status,
        @LocAddressSearchString VARCHAR(200) = @AddressSearchString

    -- Get all Clients up front to reduce table scans on the Client table and only get the ones needed. Also used by PopulateSiteJobsReinspectionState.
    CREATE TABLE #ClientIdData (ClientID INT PRIMARY KEY)
    INSERT INTO #ClientIdData (ClientID)
    SELECT LTRIM(RTRIM(s)) [ClientID]
    FROM dbo.SplitString(@LocClientIDs, ',')
    WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
    GROUP BY s

    -- Get all Sites up front to reduce table scans on the Site table and only get the ones needed.
    DECLARE @SiteIdData TABLE (SiteID INT PRIMARY KEY)
    IF @LocSiteIDs IS NOT NULL
    BEGIN
        INSERT INTO @SiteIdData (SiteID)
        SELECT LTRIM(RTRIM(s)) [SiteID]
        FROM dbo.SplitString(@LocSiteIDs, ',')
        WHERE NULLIF(LTRIM(RTRIM(s)), '') IS NOT NULL
        GROUP BY s
    END


    -- Start the main SELECT.
    SELECT TOP 100 PERCENT
        0 [SortOrder],
        j.JobNo,
        si.SiteID,
        ISNULL(si.UPRN, '') [UPRN],
        si.Address,
        si.Postcode,
        CASE
            WHEN MAX(r.DateApproved) IS NOT NULL THEN 'Approved'
            WHEN COUNT(s.SampleID) > 0 AND COUNT(s.SampleID) = COUNT(s.SampleResultID) THEN 'Sample Analysis Complete'
            WHEN COUNT(DISTINCT r.RegisterID) >= 1 THEN 'Site Work Complete'
            ELSE 'Work Scheduled'
        END [Status],
        CASE
            WHEN MAX(r.DateApproved) IS NOT NULL THEN MAX(r.DateApproved)
            WHEN COUNT(s.SampleID) > 0 AND COUNT(s.SampleID) = COUNT(s.SampleResultID) THEN MAX(CAST(s.DateAnalysed AS DATE))
            WHEN COUNT(DISTINCT r.RegisterID) >= 1 THEN MAX(CAST(r.RegisterFinish AS DATE))
            ELSE MIN(CAST(a.StartTime AS DATE))
        END [StatusDate],
        a.ClientOrderNo,
        ISNULL(MIN(sut.Description), '') [SurveyType],
        ISNULL(MIN(emp.FullName), '') [Surveyor],
        MIN(a.DateCreated) [DateCreated],
        CAST(MIN(a.DateCreated) AS DATE) [DateReceived],
        MIN(CAST(asu.DueDate AS DATE)) [DueDate],
        MIN(CAST(asu.DueDate AS DATE)) [SurveyDue],
        MIN(CAST(a.StartTime AS DATE)) [StartTime],
        MIN(CAST(a.StartTime AS DATE)) [SurveyStarted],
        MAX(CAST(r.RegisterFinish AS DATE)) [RegisterFinish],
        MAX(CAST(r.RegisterFinish AS DATE)) [SurveyCompleted],
        MAX(iat.DateCreated) [AnalysisComplete],
        MAX(iat.DateCreated) [AnalysisCompleted],
        MAX(j.Approved) [Approved],
        MAX(j.Approved) [SurveyApproved],
        ISNULL(ae.FullName, '') [ApprovedBy],
        j.JobID,
        MIN(sut.SurveyTypeID) [SurveyTypeID],
        pdfFile.FileName,
        MIN(CAST(a.StartTime AS DATE)) [AppointmentDate],
        na.NoAccessAttempts,
        na.LatestNoAccess,
        MAX(je.Created) [LatestJobEmployee],
		pc.PhoneCalls [PhoneCalls],
		l.Letters [Letters]
    FROM
        Appointment a WITH (NOLOCK)
        INNER JOIN AppointmentSurvey asu WITH (NOLOCK) ON a.AppointmentID = asu.AppointmentID
        INNER JOIN #ClientIdData cid ON a.ClientID = cid.ClientID
        INNER JOIN Project p WITH (NOLOCK) ON a.ProjectID = p.ProjectID
        INNER JOIN Site si WITH (NOLOCK) ON a.SiteID = si.SiteID
        LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
        INNER JOIN Quote q WITH (NOLOCK) ON a.QuoteID = q.QuoteID AND q.Rejected IS NULL
        INNER JOIN Job j WITH (NOLOCK) ON q.JobID = j.JobID AND j.Cancelled IS NULL
        LEFT JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
        --LEFT JOIN JobEmployee jem WITH (NOLOCK) ON j.JobID = jem.JobID AND jem.MainEmployee = 1
		OUTER APPLY
		(
			SELECT TOP 1
				FullName
			FROM
				Job j2 WITH (NOLOCK)
				INNER JOIN JobEmployee je WITH (NOLOCK) ON j.JobID = je.JobID
				INNER JOIN Employee e WITH (NOLOCK) ON je.EmployeeID = e.EmployeeID
			WHERE
				j2.JobID = j.JobID
		) emp
        --LEFT JOIN Employee se WITH (NOLOCK) ON jem.EmployeeID = se.EmployeeID
        LEFT JOIN Employee ae WITH (NOLOCK) ON j.SampleContentEmployeeID = ae.EmployeeID
        LEFT JOIN Register r WITH (NOLOCK) ON je.JobEmployeeID = r.JobEmployeeID
        LEFT JOIN Survey su WITH (NOLOCK) ON r.SurveyID = su.SurveyID
        LEFT JOIN SurveyType sut WITH (NOLOCK) ON su.SurveyTypeID = sut.SurveyTypeID
        LEFT JOIN Floorplan f WITH (NOLOCK) ON r.RegisterID = f.RegisterID
        LEFT JOIN Room rm WITH (NOLOCK) ON f.FloorplanID = rm.FloorplanID
        LEFT JOIN Sample s WITH (NOLOCK) ON rm.RoomID = s.RoomID AND s.SampleRef IS NOT NULL AND s.AsSample = 0
        LEFT JOIN IntranetAuditTrail iat WITH (NOLOCK) ON iat.DataTable = 'Job' AND iat.Message = 'Verified sample results' AND iat.DataID = j.JobID
        OUTER APPLY
        (
            SELECT TOP 1 _pf.FileName [FileName]
            FROM PDF _pf WITH (NOLOCK)
            WHERE
                _pf.JobID = j.JobID
                    AND
                _pf.DateDeleted IS NULL
                    AND
                _pf.FileName LIKE '%bsr%'
            ORDER BY
                _pf.DateCreated DESC
        ) pdfFile
        OUTER APPLY
        (
            SELECT COUNT(*) [NoAccessAttempts], MAX(Created) [LatestNoAccess]
            FROM NoAccess WITH (NOLOCK)
            WHERE
                JobID = j.JobID
                    OR
                SiteID = si.SiteID
        ) na -- No Access
		OUTER APPLY
		(
			SELECT 
				count(*) [PhoneCalls] 
			FROM SiteContact sc
			WHERE sc.SiteID = si.SiteID AND sc.ContactTypeID = 1 AND sc.Datedeleted IS NULL AND sc.ProjectID = p.ProjectID
		) pc
		OUTER APPLY
		(
			SELECT
				count(*) [Letters]
			FROM SiteContact sc
			WHERE sc.SiteID = si.SiteID AND sc.ContactTypeID = 2 AND sc.Datedeleted IS NULL AND sc.ProjectID = p.ProjectID
		) l
    WHERE
        si.Deleted IS NULL
            AND
        p.Deleted IS NULL
            AND
        a.DateDeclined IS NULL
            AND
        CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
            THEN 1
            ELSE CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
            THEN 1
            ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
            THEN 1
            ELSE
                CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString OR j.ClientOrderNo = @LocAddressSearchString
                    THEN 1
                    ELSE 0
                END
        END = 1
    GROUP BY
        a.ClientOrderNo,
        j.JobID,
        j.JobNo,
        si.SiteID,
        si.Address,
        si.Postcode,
        j.Approved,
        si.Contact,
        si.Telephone,
        si.UPRN,
        ae.FullName,
        pdfFile.FileName,
        na.NoAccessAttempts,
        na.LatestNoAccess,
		pc.PhoneCalls,
		l.Letters
    HAVING
        CASE WHEN @LocStatus IS NULL -- Status Filter.
            THEN -1
            ELSE
                CASE
                    WHEN MAX(r.DateApproved) IS NOT NULL THEN 1
                    WHEN COUNT(s.SampleID) > 0 AND COUNT(s.SampleID) = COUNT(s.SampleResultID) THEN 2
                    WHEN COUNT(DISTINCT r.RegisterID) >= 1 THEN 3
                    ELSE 4
                END
        END = ISNULL(@LocStatus, -1)

    UNION ALL

    SELECT
        1 [SortOrder],
        0 [JobNo],
        NULL [SiteID],
        ISNULL(si.UPRN, '') [UPRN],
        si.Address,
        si.Postcode,
        'Unscheduled' [Status],
        NULL [StatusDate],
        '' [ClientOrderNo],
        '' [SurveyType],
        '' [Surveyor],
        NULL [DateCreated],
        NULL [DateReceived],
        NULL [DueDate],
        NULL [SurveyDue],
        NULL [StartTime],
        NULL [SurveyStarted],
        NULL [RegisterFinish],
        NULL [SurveyCompleted],
        NULL [AnalysisComplete],
        NULL [AnalysisCompleted],
        NULL [Approved],
        NULL [SurveyApproved],
        '' [ApprovedBy],
        '' [JobID],
        '' [SurveyTypeID],
        '' [FileName],
        NULL [AppointmentDate],
        0 [NoAccessAttempts],
        NULL [LatestNoAccess],
        NULL [LatestJobEmployee],
		pc.PhoneCalls [PhoneCalls],
		l.Letters [Letters]
    FROM
        Site si WITH (NOLOCK)
        LEFT JOIN @SiteIdData siid ON si.SiteID = siid.SiteID
        INNER JOIN ClientSite cs WITH (NOLOCK) ON si.SiteID = cs.SiteID
        INNER JOIN #ClientIdData cid ON cs.ClientID = cid.ClientID
        INNER JOIN ProjectSite ps WITH (NOLOCK) ON si.SiteID = ps.SiteID
        INNER JOIN Project p WITH (NOLOCK) ON ps.ProjectID = p.ProjectID
        LEFT JOIN Appointment a WITH (NOLOCK) ON p.ProjectID = a.ProjectID AND si.SiteID = a.SiteID
		OUTER APPLY
		(
			SELECT 
				count(*) [PhoneCalls] 
			FROM SiteContact sc
			WHERE sc.SiteID = si.SiteID AND sc.ContactTypeID = 1 AND sc.Datedeleted IS NULL AND sc.ProjectID = p.ProjectID
		) pc
		OUTER APPLY
		(
			SELECT
				count(*) [Letters]
			FROM SiteContact sc
			WHERE sc.SiteID = si.SiteID AND sc.ContactTypeID = 2 AND sc.Datedeleted IS NULL AND sc.ProjectID = p.ProjectID
		) l
    WHERE
        si.Deleted IS NULL
            AND
        p.Deleted IS NULL
            AND
        a.AppointmentID IS NULL
            AND
        CASE WHEN @LocProjectGroupID IS NULL AND @LocProjectID IS NULL -- Project Filter.
            THEN 1
            ELSE CASE WHEN p.ProjectGroupID = @LocProjectGroupID OR p.ProjectID = @LocProjectID THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocSiteIDs IS NULL -- Sites Filter.
            THEN 1
            ELSE CASE WHEN siid.SiteID IS NOT NULL THEN 1 ELSE 0 END
        END = 1
            AND
        CASE WHEN @LocAddressSearchString IS NULL -- Address Filter.
            THEN 1
            ELSE
                CASE WHEN si.Address LIKE '%' + @LocAddressSearchString + '%' OR si.Postcode LIKE '%' + @LocAddressSearchString + '%' OR si.Address + ', ' + ISNULL(si.Postcode, '') LIKE '%' + @LocAddressSearchString + '%' OR si.UPRN = @LocAddressSearchString /*OR j.ClientOrderNo = @LocAddressSearchString*/
                    THEN 1
                    ELSE 0
                END
        END = 1
            AND
        CASE WHEN @LocStatus IS NULL -- Status Filter.
            THEN 1
            ELSE CASE WHEN @LocStatus = 5 THEN 1 ELSE 0 END
        END = 1
    GROUP BY
        si.Address,
        si.Postcode,
        si.UPRN,
		pc.PhoneCalls,
		l.Letters
    ORDER BY
        SortOrder,
        JobNo,
        Address

    -- Clear up temp tables.
    DROP TABLE #ClientIdData


    SET NOCOUNT OFF;
END
GO

USE [TEAMS]
GO

/****** Object:  StoredProcedure [dbo].[AutomaticEmail_PortalUserForSite]    Script Date: 05/14/2018 16:08:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER PROCEDURE [dbo].[AutomaticEmail_PortalUserForSite]
    @PortalUserAutoEmailTypeID INT = NULL
/*
    For the Portal Automatic Email functionality, get the data that the Client can access. However, we only get the SiteID if we are want emails for this Site.
*/
AS
BEGIN
    SET NOCOUNT ON;


    -- Start the main SELECT.
    SELECT
        pu.PortalUserId [PortalUserID],
        pu.Email + ';' + pu.FullName [EMAILRECIPIENT],
        dbo.FormatTeamsDate(GETDATE(), 1) [EmailDate],
        si.Address + ISNULL(', ' + NULLIF(si.Postcode, ''), '') [SiteAddressWithPostcode],
        c.ClientIDs,
        ISNULL(pu.ProjectGroupID, 0) [ProjectGroupID],
        ISNULL(pu.ProjectID, 0) [ProjectID],
        si.SiteID [SiteID]
    FROM
        PortalUserAutoEmail puae WITH (NOLOCK)
        INNER JOIN PortalUserAutoEmailType puaet WITH (NOLOCK) ON puae.PortalUserAutoEmailTypeID = puaet.PortalUserAutoEmailTypeID
        INNER JOIN PortalUser pu WITH (NOLOCK) ON puae.PortalUserID = pu.PortalUserId AND pu.Deleted IS NULL
        INNER JOIN Site si WITH (NOLOCK) ON puae.SiteID = si.SiteID
        OUTER APPLY
        (
            SELECT STUFF((
                SELECT ',' + CAST(c.ClientID AS VARCHAR(50))
                FROM
                    ClientPortalUser cpu WITH (NOLOCK)
                    INNER JOIN Client c WITH (NOLOCK) ON cpu.ClientId = c.ClientID
                WHERE cpu.PortalUserId = pu.PortalUserId AND c.Deleted IS NULL
                GROUP BY c.ClientID
            FOR XML PATH(''), TYPE).value('.', 'VARCHAR(MAX)'), 1, 1, '') [ClientIDs]
        ) c
    WHERE
        puae.PortalUserAutoEmailTypeID = @PortalUserAutoEmailTypeID
            AND
        puaet.Deleted IS NULL
            AND
        pu.Email LIKE '%@%' -- Add basic validation to make sure it's an email.


    SET NOCOUNT OFF;
END

GO


