/* Keep a holiday calendar right: the prototype's screens, drawn from the query string. */
'use strict';

// ---------------------------------------------------------------------------
// Mock data. The QuantLib calendars are the Acme seed as the server returned
// them on 2026-10-25; the three user calendars are invented.
// ---------------------------------------------------------------------------
const SEED_CALENDARS = [
    {"code": "Argentina", "name": "Argentina", "calendar_type": "public_holiday", "country_code": "AR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Australia", "name": "Australia", "calendar_type": "public_holiday", "country_code": "AU", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Austria", "name": "Austria", "calendar_type": "public_holiday", "country_code": "AT", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Botswana", "name": "Botswana", "calendar_type": "public_holiday", "country_code": "BW", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Brazil", "name": "Brazil", "calendar_type": "public_holiday", "country_code": "BR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Canada.Settlement", "name": "Canada (Settlement)", "calendar_type": "public_holiday", "country_code": "CA", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Canada.TSX", "name": "Canada (Toronto Stock Exchange)", "calendar_type": "financial_centre", "country_code": "CA", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Chile", "name": "Chile", "calendar_type": "public_holiday", "country_code": "CL", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "China.IB", "name": "China (Interbank)", "calendar_type": "financial_centre", "country_code": "CN", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "China.SSE", "name": "China (Shanghai Stock Exchange)", "calendar_type": "financial_centre", "country_code": "CN", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "CzechRepublic", "name": "Czech Republic", "calendar_type": "public_holiday", "country_code": "CZ", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Denmark", "name": "Denmark", "calendar_type": "public_holiday", "country_code": "DK", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Finland", "name": "Finland", "calendar_type": "public_holiday", "country_code": "FI", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "France", "name": "France", "calendar_type": "public_holiday", "country_code": "FR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Germany.Eurex", "name": "Germany (Eurex)", "calendar_type": "financial_centre", "country_code": "DE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Germany.Euwax", "name": "Germany (Euwax)", "calendar_type": "financial_centre", "country_code": "DE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Germany.FrankfurtStockExchange", "name": "Germany (Frankfurt Stock Exchange)", "calendar_type": "financial_centre", "country_code": "DE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Germany.Settlement", "name": "Germany (Settlement)", "calendar_type": "public_holiday", "country_code": "DE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Germany.Xetra", "name": "Germany (Xetra)", "calendar_type": "financial_centre", "country_code": "DE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "HongKong", "name": "Hong Kong", "calendar_type": "public_holiday", "country_code": "HK", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Hungary", "name": "Hungary", "calendar_type": "public_holiday", "country_code": "HU", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Iceland", "name": "Iceland", "calendar_type": "public_holiday", "country_code": "IS", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "India", "name": "India (National Stock Exchange)", "calendar_type": "financial_centre", "country_code": "IN", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Indonesia.BEJ", "name": "Indonesia (Jakarta SE, legacy BEJ)", "calendar_type": "financial_centre", "country_code": "ID", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Indonesia.IDX", "name": "Indonesia (Indonesia Stock Exchange)", "calendar_type": "financial_centre", "country_code": "ID", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Indonesia.JSX", "name": "Indonesia (Jakarta SE, legacy JSX)", "calendar_type": "financial_centre", "country_code": "ID", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Israel.Settlement", "name": "Israel (Settlement)", "calendar_type": "public_holiday", "country_code": "IL", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Israel.TASE", "name": "Israel (Tel-Aviv Stock Exchange)", "calendar_type": "financial_centre", "country_code": "IL", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Italy", "name": "Italy", "calendar_type": "public_holiday", "country_code": "IT", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Japan", "name": "Japan", "calendar_type": "public_holiday", "country_code": "JP", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Mexico", "name": "Mexico", "calendar_type": "public_holiday", "country_code": "MX", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "NewZealand", "name": "New Zealand", "calendar_type": "public_holiday", "country_code": "NZ", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Norway", "name": "Norway", "calendar_type": "public_holiday", "country_code": "NO", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Poland", "name": "Poland", "calendar_type": "public_holiday", "country_code": "PL", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Romania", "name": "Romania", "calendar_type": "public_holiday", "country_code": "RO", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Russia", "name": "Russia", "calendar_type": "public_holiday", "country_code": "RU", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "SaudiArabia", "name": "Saudi Arabia", "calendar_type": "public_holiday", "country_code": "SA", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Singapore", "name": "Singapore", "calendar_type": "public_holiday", "country_code": "SG", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Slovakia", "name": "Slovakia", "calendar_type": "public_holiday", "country_code": "SK", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "SouthAfrica", "name": "South Africa", "calendar_type": "public_holiday", "country_code": "ZA", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "SouthKorea.KRX", "name": "South Korea (Korea Exchange)", "calendar_type": "financial_centre", "country_code": "KR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "SouthKorea.Settlement", "name": "South Korea (Settlement)", "calendar_type": "public_holiday", "country_code": "KR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Sweden", "name": "Sweden", "calendar_type": "public_holiday", "country_code": "SE", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Switzerland", "name": "Switzerland", "calendar_type": "public_holiday", "country_code": "CH", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Taiwan", "name": "Taiwan", "calendar_type": "public_holiday", "country_code": "TW", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "TARGET", "name": "TARGET (Euro area)", "calendar_type": "public_holiday", "country_code": "ZZ", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Thailand", "name": "Thailand", "calendar_type": "public_holiday", "country_code": "TH", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Turkey", "name": "Turkey", "calendar_type": "public_holiday", "country_code": "TR", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "Ukraine", "name": "Ukraine", "calendar_type": "public_holiday", "country_code": "UA", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedKingdom.Exchange", "name": "United Kingdom (Exchange)", "calendar_type": "financial_centre", "country_code": "GB", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedKingdom.Metals", "name": "United Kingdom (Metals)", "calendar_type": "financial_centre", "country_code": "GB", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedKingdom.Settlement", "name": "United Kingdom (Settlement)", "calendar_type": "public_holiday", "country_code": "GB", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.FederalReserve", "name": "United States (Federal Reserve)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.GovernmentBond", "name": "United States (Government Bond)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.LiborImpact", "name": "United States (Libor Impact)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.NERC", "name": "United States (NERC)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.NYSE", "name": "United States (NYSE)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.Settlement", "name": "United States (Settlement)", "calendar_type": "public_holiday", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "UnitedStates.SOFR", "name": "United States (SOFR)", "calendar_type": "financial_centre", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "US.FOMC", "name": "US Federal Reserve (FOMC meeting calendar)", "calendar_type": "central_bank_meeting", "country_code": "US", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"},
    {"code": "WeekendsOnly", "name": "Weekends Only", "calendar_type": "public_holiday", "country_code": "ZZ", "source": "quantlib", "is_editable": false, "base_calendar_code": null, "version": 1, "modified_by": "ores_brave_hopper_refdata_service"}
];

const USER_CALENDARS = [
    {
        code: 'Acme.Desk', name: 'Acme desk calendar', calendar_type: 'public_holiday', country_code: 'GB',
        source: 'user', is_editable: true, base_calendar_code: null, version: 4,
        modified_by: 'priya.natarajan', recorded_at: '2026-10-21T09:12:00Z',
        change_reason_code: 'user.booking_error', change_commentary: 'The spring bank holiday rule was a week out.',
    },
    {
        code: 'Acme.London', name: 'Acme London office', calendar_type: 'public_holiday', country_code: 'GB',
        source: 'user', is_editable: true, base_calendar_code: 'UnitedKingdom.Settlement', version: 2,
        modified_by: 'adrian.vance', recorded_at: '2026-10-18T15:40:00Z',
        change_reason_code: 'system.new_record', change_commentary: '',
    },
    {
        code: 'Acme.NewYork', name: 'Acme New York office', calendar_type: 'financial_centre', country_code: 'US',
        source: 'user', is_editable: true, base_calendar_code: 'UnitedStates.Settlement', version: 1,
        modified_by: 'adrian.vance', recorded_at: '2026-10-18T15:52:00Z',
        change_reason_code: 'system.new_record', change_commentary: '',
    },
];

const RULES = {
    'Acme.Desk': [
        { id: 'r1', kind: 'fixed_date', month: 1, day: 1, shift: 'roll_forward_to_monday', effective_from: null, effective_to: null, version: 1 },
        { id: 'r2', kind: 'easter_offset', day_offset: -2, shift: 'none', effective_from: null, effective_to: null, version: 1 },
        { id: 'r3', kind: 'easter_offset', day_offset: 1, shift: 'none', effective_from: null, effective_to: null, version: 1 },
        { id: 'r4', kind: 'nth_weekday_of_month', month: 5, weekday: 1, occurrence: 1, shift: 'none', effective_from: null, effective_to: null, version: 1 },
        { id: 'r5', kind: 'last_weekday_of_month', month: 5, weekday: 1, shift: 'none', effective_from: null, effective_to: null, version: 2 },
        { id: 'r6', kind: 'last_weekday_of_month', month: 8, weekday: 1, shift: 'none', effective_from: null, effective_to: null, version: 1 },
        { id: 'r7', kind: 'fixed_date', month: 12, day: 25, shift: 'roll_forward_to_monday', effective_from: null, effective_to: null, version: 1 },
        { id: 'r8', kind: 'fixed_date', month: 12, day: 26, shift: 'roll_forward_to_monday', effective_from: null, effective_to: null, version: 1 },
        { id: 'r9', kind: 'fixed_date', month: 11, day: 5, shift: 'none', effective_from: 2019, effective_to: 2024, version: 1 },
    ],
    'Acme.London': [],
    'Acme.NewYork': [],
};

const EXCEPTIONS = {
    'Acme.Desk': [
        { id: 'x1', exception_date: '2026-06-12', is_business_day: false, description: 'Office closed for the move', version: 1 },
        { id: 'x2', exception_date: '2026-05-25', is_business_day: true, description: 'Desk open on the spring bank holiday', version: 1 },
    ],
    'Acme.London': [
        { id: 'x3', exception_date: '2026-06-12', is_business_day: false, description: 'Office closed for the move', version: 1 },
        { id: 'x4', exception_date: '2026-12-24', is_business_day: false, description: 'Christmas Eve, office closed', version: 1 },
    ],
    'Acme.NewYork': [
        { id: 'x5', exception_date: '2026-11-27', is_business_day: false, description: 'Day after Thanksgiving', version: 1 },
    ],
};

const EVENTS = {
    'Acme.Desk': [
        { id: 'e1', event_date: '2026-02-05', diary_entry_type: 'central_bank_meeting', name: 'MPC decision', description: 'Bank of England rate decision', version: 1 },
        { id: 'e2', event_date: '2026-03-19', diary_entry_type: 'central_bank_meeting', name: 'MPC decision', description: 'Bank of England rate decision', version: 1 },
    ],
    'Acme.London': [],
    'Acme.NewYork': [],
};

/* QuantLib holidays of the two bases the user calendars derive from, for 2026. The
   server does not read them today (the journey's gap 2); the prototype draws the
   intended screen, in which it does. */
const BASE_HOLIDAYS = {
    'UnitedKingdom.Settlement': ['2026-01-01', '2026-04-03', '2026-04-06', '2026-05-04', '2026-05-25', '2026-08-31', '2026-12-25', '2026-12-28'],
    'UnitedStates.Settlement': ['2026-01-01', '2026-01-19', '2026-02-16', '2026-05-25', '2026-06-19', '2026-07-03', '2026-09-07', '2026-10-12', '2026-11-11', '2026-11-26', '2026-12-25'],
    TARGET: ['2026-01-01', '2026-04-03', '2026-04-06', '2026-05-01', '2026-12-25'],
};

/* The history of a record is one stream of events, newest first, as the Entity
   screen specification's History and the timeline section draws it. */
const HISTORY = {
    'Acme.Desk': [
        { version: 4, kind: 'Changed', at: '2026-10-21T09:12:40Z', by: 'priya.natarajan', service: false, reason: 'user.booking_error', note: 'The spring bank holiday rule was a week out.', changes: [['Name', 'Acme desk', 'Acme desk calendar']] },
        { version: 3, kind: 'Changed', at: '2026-10-09T16:02:11Z', by: 'priya.natarajan', service: false, reason: 'business.data_not_updated', note: 'Added the summer bank holiday.', changes: [['Calendar type', 'financial_centre', 'public_holiday']] },
        { version: 2, kind: 'Changed', at: '2026-10-02T11:30:05Z', by: 'adrian.vance', service: false, reason: 'user.booking_error', note: 'Wrong country.', changes: [['Country', 'IE', 'GB']] },
        { version: 1, kind: 'Created', at: '2026-10-02T11:14:52Z', by: 'adrian.vance', service: false, reason: 'system.new_record', note: '', changes: [] },
    ],
    'Acme.London': [
        { version: 2, kind: 'Changed', at: '2026-10-18T15:40:19Z', by: 'ores_brave_hopper_refdata_service', service: true, reason: 'system.external_data_import', note: 'Bumped by child calendar_exception: Christmas Eve, office closed', changes: [] },
        { version: 1, kind: 'Created', at: '2026-10-18T15:38:02Z', by: 'adrian.vance', service: false, reason: 'system.new_record', note: 'Derived from UnitedKingdom.Settlement', changes: [] },
    ],
};
const PART_HISTORY = {
    r5: [
        { version: 2, kind: 'Changed', at: '2026-10-21T09:11:58Z', by: 'priya.natarajan', service: false, reason: 'user.booking_error', note: 'The spring bank holiday is the last Monday, not the first.', changes: [['Occurrence', '1st', '—'], ['Kind', 'Nth weekday of the month', 'Last weekday of the month']] },
        { version: 1, kind: 'Created', at: '2026-10-02T11:20:30Z', by: 'adrian.vance', service: false, reason: 'system.new_record', note: '', changes: [] },
    ],
};
const SEED_EVENT = { version: 1, kind: 'Created', at: '2026-10-25T11:04:42Z', by: 'ores_brave_hopper_refdata_service', service: true, reason: 'system.external_data_import', note: 'Imported from DQ dataset: QuantLib calendars', changes: [] };
const KIND_COLOUR = { Created: '#8957e5', Changed: '#58a6ff' };

const CALENDAR_TYPES = { public_holiday: 'Public holiday', financial_centre: 'Financial centre', central_bank_meeting: 'Central bank meeting' };
const DIARY_TYPES = { central_bank_meeting: 'Central bank meeting', fixing: 'Fixing', other: 'Other' };
const COUNTRIES = { GB: 'United Kingdom', US: 'United States', IE: 'Ireland', DE: 'Germany', FR: 'France', JP: 'Japan', ZZ: 'No country' };
const REASONS = ['Non-material update (Touch)', 'User/Booking Error', 'Duplicate Record', 'Data not updated within required liquidity horizon', 'Manual override after plausibility check failure', 'Upstream vendor/API data issue', 'Mandatory compliance adjustment', 'Exceptional (requires audit note)'];
const WEEKDAYS = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
const ORDINAL = { 1: '1st', 2: '2nd', 3: '3rd', 4: '4th' };
const ALL = [...USER_CALENDARS, ...SEED_CALENDARS];
const LOADED = '2026-10-25T14:20:11+00:00';

// ---------------------------------------------------------------------------
// The query string is the state.
// ---------------------------------------------------------------------------
const q = new URLSearchParams(location.search);
const state = {
    screen: q.get('screen') ?? 'list',
    code: q.get('code') ?? '',
    tab: q.get('tab') ?? 'details',
    dialog: q.get('dialog') ?? '',
    part: q.get('part') ?? '',
    who: q.get('who') ?? 'admin',
    year: /^\d{4}$/.test(q.get('year') ?? '') ? Number(q.get('year')) : 2026,
    compare: q.get('compare') ?? '',
    built: q.get('built') ?? 'yes',
    search: q.get('q') ?? '',
    page: Number(q.get('page') ?? '1'),
    size: Number(q.get('size') ?? '15'),
    sort: q.get('sort') ?? 'code',
    dir: q.get('dir') ?? 'asc',
    guard: q.get('guard') === '1',
    done: q.get('done') ?? '',
};
const admin = state.who === 'admin';

function href(changes) {
    const next = new URLSearchParams(location.search);
    for (const [k, v] of Object.entries(changes)) {
        if (v === '' || v === null || v === undefined) next.delete(k); else next.set(k, String(v));
    }
    const s = next.toString();
    return s === '' ? location.pathname : `?${s}`;
}
function go(changes) { location.href = href(changes); }
function esc(s) { return String(s ?? '').replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c])); }
function icon(name) {
    const paths = {
        refresh: 'M4 4v5h5M20 20v-5h-5M5.6 15A7 7 0 0 0 18.4 9M18.4 15A7 7 0 0 1 5.6 9',
        add: 'M12 5v14M5 12h14',
        edit: 'M4 20h4l10-10-4-4L4 16v4zM13 7l4 4',
        delete: 'M4 7h16M9 7V4h6v3M6 7l1 13h10l1-13',
        save: 'M5 3h11l3 3v15H5zM8 3v6h7V3M8 21v-7h8v7',
        cancel: 'M6 6l12 12M18 6L6 18',
        history: 'M3 12a9 9 0 1 0 3-6.7M3 4v5h5M12 7v5l3 3',
        derive: 'M6 4v16M6 12h8a4 4 0 0 0 4-4V4M18 4l-2 2 2 2',
        rebuild: 'M3 3h18v18H3zM3 9h18M9 3v18M14 14h3v3',
        left: 'M15 6l-6 6 6 6', right: 'M9 6l6 6-6 6',
        revert: 'M3 10h10a5 5 0 1 1 0 10H9M3 10l4-4M3 10l4 4',
        search: 'M11 4a7 7 0 1 1 0 14 7 7 0 0 1 0-14zM20 20l-4-4',
    };
    return `<svg class="icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="${paths[name]}"/></svg>`;
}
function flag(code, small) { return `<span class="flag${small ? ' sm' : ''}" aria-hidden="true">${esc(code.slice(0, 3).toUpperCase())}</span>`; }
function shapeOf(c) { return c.is_editable !== true ? 'quantlib' : (c.base_calendar_code ? 'derived' : 'bespoke'); }
function madeOf(c) {
    const shape = shapeOf(c);
    if (shape === 'quantlib') return 'QuantLib';
    if (shape === 'bespoke') return 'Its own rules';
    return `${c.base_calendar_code}, with exceptions`;
}
function find(code) { return ALL.find((c) => c.code === code); }
/* ES-REF-01: a code that names another record is a link to its page, with its flag.
   The country page is the address the countries record kind will answer at. */
function countryHref(code) { return `/refdata/countries/${encodeURIComponent(code)}`; }
function tabsOf(c) {
    const s = shapeOf(c);
    if (s === 'quantlib') return ['details', 'events', 'history'];
    if (s === 'derived') return ['details', 'exceptions', 'events', 'days', 'history'];
    return ['details', 'rules', 'exceptions', 'events', 'days', 'history'];
}
const TAB_NAMES = { details: 'Details', rules: 'Rules', exceptions: 'Exceptions', events: 'Diary events', days: 'Business days', history: 'History' };

// ---------------------------------------------------------------------------
// Rules to dates. Enough of the engine to make the Business days view real.
// ---------------------------------------------------------------------------
function easter(year) {
    const a = year % 19, b = Math.floor(year / 100), c = year % 100, d = Math.floor(b / 4), e = b % 4;
    const f = Math.floor((b + 8) / 25), g = Math.floor((b - f + 1) / 3), h = (19 * a + b - d - g + 15) % 30;
    const i = Math.floor(c / 4), k = c % 4, l = (32 + 2 * e + 2 * i - h - k) % 7, m = Math.floor((a + 11 * h + 22 * l) / 451);
    const month = Math.floor((h + l - 7 * m + 114) / 31), day = ((h + l - 7 * m + 114) % 31) + 1;
    return new Date(Date.UTC(year, month - 1, day));
}
function iso(d) { return d.toISOString().slice(0, 10); }
function addDays(d, n) { const x = new Date(d); x.setUTCDate(x.getUTCDate() + n); return x; }
function shift(d, how) {
    const wd = d.getUTCDay();
    if (how === 'roll_forward_to_monday') { if (wd === 6) return addDays(d, 2); if (wd === 0) return addDays(d, 1); }
    if (how === 'nearest_weekday') { if (wd === 6) return addDays(d, -1); if (wd === 0) return addDays(d, 1); }
    return d;
}
function ruleDate(rule, year) {
    if (rule.effective_from && year < rule.effective_from) return null;
    if (rule.effective_to && year > rule.effective_to) return null;
    let d;
    if (rule.kind === 'fixed_date') d = new Date(Date.UTC(year, rule.month - 1, rule.day));
    else if (rule.kind === 'easter_offset') d = addDays(easter(year), rule.day_offset);
    else if (rule.kind === 'nth_weekday_of_month') {
        const first = new Date(Date.UTC(year, rule.month - 1, 1));
        const delta = (rule.weekday - first.getUTCDay() + 7) % 7;
        d = addDays(first, delta + 7 * (rule.occurrence - 1));
    } else {
        const last = new Date(Date.UTC(year, rule.month, 0));
        const delta = (last.getUTCDay() - rule.weekday + 7) % 7;
        d = addDays(last, -delta);
    }
    return iso(shift(d, rule.shift));
}
function ruleWords(rule) {
    if (rule.kind === 'fixed_date') return `${rule.day} ${MONTHS[rule.month - 1]}`;
    if (rule.kind === 'nth_weekday_of_month') return `${ORDINAL[rule.occurrence]} ${WEEKDAYS[rule.weekday]} of ${MONTHS[rule.month - 1]}`;
    if (rule.kind === 'last_weekday_of_month') return `Last ${WEEKDAYS[rule.weekday]} of ${MONTHS[rule.month - 1]}`;
    const o = rule.day_offset;
    return `${o > 0 ? '+' + o : o} days from Easter`;
}
function years(rule) {
    if (!rule.effective_from && !rule.effective_to) return '—';
    return `${rule.effective_from ?? '…'} – ${rule.effective_to ?? '…'}`;
}
/* The holidays one calendar produces in a year, each with what produced it. */
function holidays(c, year) {
    const out = new Map();
    const shape = shapeOf(c);
    const inYear = (dates) => (dates ?? []).filter((d) => d.startsWith(String(year)));
    if (shape === 'quantlib') for (const d of inYear(BASE_HOLIDAYS[c.code])) out.set(d, { words: 'QuantLib', from: 'base' });
    if (shape === 'derived') for (const d of inYear(BASE_HOLIDAYS[c.base_calendar_code])) out.set(d, { words: `From ${c.base_calendar_code}`, from: 'base' });
    if (shape === 'bespoke') for (const r of RULES[c.code] ?? []) { const d = ruleDate(r, year); if (d) out.set(d, { words: ruleWords(r), from: 'rule' }); }
    for (const x of EXCEPTIONS[c.code] ?? []) {
        if (!x.exception_date.startsWith(String(year))) continue;
        if (x.is_business_day) out.set(x.exception_date, { words: x.description || 'Exception', from: 'business' });
        else out.set(x.exception_date, { words: x.description || 'Exception', from: 'exception' });
    }
    return out;
}
function inYearCount(c, year) {
    const dates = BASE_HOLIDAYS[shapeOf(c) === 'derived' ? c.base_calendar_code : c.code] ?? [];
    return dates.filter((d) => d.startsWith(String(year))).length;
}
function weekdaysIn(year) { let n = 0; for (let d = new Date(Date.UTC(year, 0, 1)); d.getUTCFullYear() === year; d = addDays(d, 1)) { const w = d.getUTCDay(); if (w !== 0 && w !== 6) n++; } return n; }

// ---------------------------------------------------------------------------
// Screens
// ---------------------------------------------------------------------------
const main = document.getElementById('main');
const layer = document.getElementById('layer');

function crumbs(items) {
    return `<nav class="crumbs" aria-label="Breadcrumb">${items.map((it, i) => (it.href ? `<a href="${it.href}">${esc(it.label)}</a>` : `<span>${esc(it.label)}</span>`) + (i < items.length - 1 ? ' <span>›</span>' : '')).join(' ')}</nav>`;
}
function refreshButton() {
    return `<div class="actions-col"><button class="btn" type="button">${icon('refresh')} Refresh</button><span class="loaded">${esc(LOADED)}</span></div>`;
}

function renderList() {
    document.title = 'ORE Studio Holiday calendars';
    const search = state.search.trim().toLowerCase();
    let rows = ALL.filter((c) => !search || c.code.toLowerCase().includes(search) || c.name.toLowerCase().includes(search) || (COUNTRIES[c.country_code] ?? '').toLowerCase().includes(search));
    const key = state.sort;
    rows.sort((a, b) => String(a[key] ?? '').localeCompare(String(b[key] ?? '')) * (state.dir === 'desc' ? -1 : 1));
    const total = rows.length;
    const from = (state.page - 1) * state.size;
    const pageRows = rows.slice(from, from + state.size);
    const pages = Math.max(1, Math.ceil(total / state.size));
    const th = (id, label) => `<th><button type="button" ${state.sort === id ? `aria-sort="${state.dir === 'asc' ? 'ascending' : 'descending'}"` : ''} onclick="go({sort:'${id}',dir:'${state.sort === id && state.dir === 'asc' ? 'desc' : 'asc'}',page:''})">${label}${state.sort === id ? (state.dir === 'asc' ? ' ↑' : ' ↓') : ''}</button></th>`;
    main.innerHTML = `
      ${crumbs([{ label: 'Home', href: '#' }, { label: 'Reference data', href: '#' }, { label: 'Holiday calendars' }])}
      <div class="head">
        <div class="titles"><h1>Holiday calendars</h1><p class="lead">The calendars that decide which days are business days, for settlement and for schedules.</p></div>
        <div class="actions">${refreshButton()}${admin ? `<a class="btn primary" href="${href({ dialog: 'add' })}">${icon('add')} Add calendar</a>` : ''}</div>
      </div>
      <section class="card">
        <div class="toolbar">
          <form class="search" role="search" onsubmit="event.preventDefault(); go({q: this.q.value, page: ''})">${icon('search')}<input type="search" name="q" placeholder="Search…" value="${esc(state.search)}" aria-label="Search">${state.search ? `<button type="button" aria-label="Clear the search" onclick="go({q:'',page:''})">✕</button>` : ''}</form>
          <button class="btn ghost sm" type="button">▸ Columns</button>
        </div>
        <div class="table-wrap"><table>
          <thead><tr>${th('code', 'Code')}${th('name', 'Name')}${th('calendar_type', 'Calendar type')}${th('country_code', 'Country')}<th>Holidays from</th></tr></thead>
          <tbody>${pageRows.length === 0 ? `<tr><td colspan="5"><div class="empty">No calendars match the search. <a href="${href({ q: '', page: '' })}">Clear the search</a></div></td></tr>` : pageRows.map((c) => `
            <tr class="open" onclick="go({screen:'calendar',code:'${c.code}',tab:'',q:'',page:'',sort:'',dir:''})">
              <td class="code"><span class="cell-flag">${flag(c.country_code, true)}${esc(c.code)}</span></td>
              <td>${esc(c.name)}</td>
              <td>${esc(CALENDAR_TYPES[c.calendar_type] ?? c.calendar_type)}</td>
              <td><span class="cell-flag">${flag(c.country_code, true)}<a href="${countryHref(c.country_code)}" onclick="event.stopPropagation()">${esc(COUNTRIES[c.country_code] ?? c.country_code)}</a></span></td>
              <td class="muted">${esc(madeOf(c))}</td>
            </tr>`).join('')}</tbody>
        </table></div>
        <div class="foot">
          <span class="num">${total === 0 ? '0–0 of 0' : `${from + 1}–${Math.min(from + state.size, total)} of ${total}`}</span>
          <span class="pager">
            <a class="btn sm" href="${href({ page: '' })}" ${state.page === 1 ? 'aria-disabled="true"' : ''}>First</a>
            <a class="btn sm" href="${href({ page: state.page > 2 ? state.page - 1 : '' })}" ${state.page === 1 ? 'aria-disabled="true"' : ''}>Previous</a>
            <a class="btn sm" href="${href({ page: Math.min(pages, state.page + 1) })}" ${state.page >= pages ? 'aria-disabled="true"' : ''}>Next</a>
            <a class="btn sm" href="${href({ page: pages })}" ${state.page >= pages ? 'aria-disabled="true"' : ''}>Last</a>
            <label>Page size <select onchange="go({size:this.value,page:''})">${[15, 30, 50].map((n) => `<option ${n === state.size ? 'selected' : ''}>${n}</option>`).join('')}</select></label>
          </span>
        </div>
      </section>`;
    if (state.done) doneToast();
    else if (state.dialog === 'add') dialogCalendar('Add a bespoke calendar', null, null);
}

/* The one notice a write leaves behind: the dialog has closed and Refresh is the way to the new version. */
function doneToast() {
    const to = /^\d{4}$/.test(q.get('to') ?? '') ? Number(q.get('to')) : 2027;
    let written = 0;
    for (let y = 2027; y <= to; y++) written += weekdaysIn(y);
    const words = {
        saved: 'Saved. Refresh shows the new version.',
        added: 'Added. The page opened on the new record.',
        rebuilt: `${written} days written, up to the end of ${to}.`,
        removed: 'Removed. Its history stays.',
        reverted: 'Reverted: a new version carries the old values.',
    }[state.done];
    layer.innerHTML = `<div class="notice ok toast">${esc(words)} <a href="${href({ done: '', to: '' })}">Dismiss</a></div>`;
}

function renderCalendar() {
    const c = find(state.code);
    if (!c) { main.innerHTML = `<div class="notice error">There is no calendar ${esc(state.code)}. <a href="${href({ screen: 'list', code: '' })}">Back to the calendars</a></div>`; return; }
    const shape = shapeOf(c);
    const editable = shape !== 'quantlib';
    const tabs = tabsOf(c);
    const tab = tabs.includes(state.tab) ? state.tab : 'details';
    document.title = `ORE Studio ${c.name}`;
    const own = admin ? `
        <a class="btn" href="${href({ dialog: 'derive' })}">${icon('derive')} Derive a calendar</a>
        ${editable ? `<a class="btn" href="${href({ dialog: 'rebuild' })}">${icon('rebuild')} Rebuild business days</a>` : ''}` : '';
    main.innerHTML = `
      ${crumbs([{ label: 'Home', href: '#' }, { label: 'Reference data', href: '#' }, { label: 'Holiday calendars', href: href({ screen: 'list', code: '', tab: '', dialog: '' }) }, { label: c.code }])}
      <div class="head">
        <div class="titles">
          <h1>${flag(c.country_code)}${esc(c.name)}</h1>
          <p class="lead"><span class="mono">${esc(c.code)}</span> · version ${c.version}</p>
        </div>
        <div class="actions">
          ${refreshButton()}
          <span class="mark${admin && editable ? '' : ' ro'}">${admin && editable ? `${icon('edit')} Editable` : 'Read only'}</span>
          ${own}
          ${admin && editable ? `<a class="btn" href="${href({ dialog: 'edit' })}">${icon('edit')} Edit</a><a class="btn danger" href="${href({ dialog: 'delete' })}">${icon('delete')} Delete</a>` : ''}
        </div>
      </div>
      <nav class="tabs" role="tablist">${tabs.map((t) => `<a role="tab" aria-selected="${t === tab}" href="${href({ tab: t, dialog: '', part: '' })}">${TAB_NAMES[t]}</a>`).join('')}</nav>
      <div id="tab"></div>`;
    const el = document.getElementById('tab');
    if (tab === 'details') el.innerHTML = details(c);
    else if (tab === 'rules') el.innerHTML = partsPanel(c, 'rules');
    else if (tab === 'exceptions') el.innerHTML = partsPanel(c, 'exceptions');
    else if (tab === 'events') el.innerHTML = partsPanel(c, 'events');
    else if (tab === 'days') el.innerHTML = daysPanel(c);
    else el.innerHTML = historyPanel(c);
    dialogs(c);
}

function details(c) {
    const shape = shapeOf(c);
    const base = c.base_calendar_code ? find(c.base_calendar_code) : null;
    const made = base ? `<a href="${href({ code: base.code, tab: '', dialog: '' })}">${esc(madeOf(c))}</a>` : esc(madeOf(c));
    return `
      <section class="card">
        <div class="pad"><dl>
          <div><dt>Code</dt><dd><span class="cell-flag">${flag(c.country_code, true)}<span class="mono">${esc(c.code)}</span></span><span class="help">Set when the record was made, and never changed here.</span></dd></div>
          <div><dt>Name</dt><dd>${esc(c.name)}</dd></div>
          <div><dt>Calendar type</dt><dd><a href="#">${esc(CALENDAR_TYPES[c.calendar_type] ?? c.calendar_type)}</a></dd></div>
          <div><dt>Country</dt><dd><span class="cell-flag">${flag(c.country_code, true)}<a href="${countryHref(c.country_code)}" title="Open ${esc(COUNTRIES[c.country_code] ?? c.country_code)}">${esc(COUNTRIES[c.country_code] ?? c.country_code)}</a></span></dd></div>
          <div><dt>Holidays from</dt><dd>${made}</dd></div>
          <div><dt>Flag</dt><dd>${flag(c.country_code)}</dd></div>
        </dl></div>
        <div class="last-changed">Last changed: <span class="mono">version ${c.version}</span> · ${esc(c.modified_by)} · ${c.modified_by.includes('service') ? '3 hours ago' : '4 days ago'} · <span class="mono">${esc(c.change_reason_code ?? 'system.external_data_import')}</span>${c.change_commentary ? ` · “${esc(c.change_commentary)}”` : ''}</div>
      </section>
      ${shape === 'quantlib' ? `<div class="notice info">QuantLib supplies the holidays of this calendar, and they are not stored here. To add your own holidays or business days, derive a calendar from it.</div>` : ''}`;
}

const PART = {
    rules: { title: 'rule', add: 'Add a rule', lead: 'A rule is a holiday that recurs every year. The fields follow its kind.', columns: ['Rule', 'Kind', 'Shift', 'Years'], row: (r) => [esc(ruleWords(r)), esc({ fixed_date: 'Fixed date', nth_weekday_of_month: 'Nth weekday of the month', last_weekday_of_month: 'Last weekday of the month', easter_offset: 'Easter offset' }[r.kind]), esc({ none: 'None', nearest_weekday: 'Nearest weekday', roll_forward_to_monday: 'Roll forward to Monday' }[r.shift]), `<span class="num">${esc(years(r))}</span>`] },
    exceptions: { title: 'exception', add: 'Add an exception', lead: 'An exception changes one date: a holiday on a working day, or a business day on a holiday. To stop a recurring holiday from a year on, set the last year of its rule.', columns: ['Date', 'Effect', 'Description'], row: (x) => [`<span class="mono">${x.exception_date}</span>`, x.is_business_day ? '<span class="badge business">Business day</span>' : '<span class="badge holiday">Holiday</span>', esc(x.description || '—')] },
    events: { title: 'diary event', add: 'Add a diary event', lead: 'A diary event marks a date, such as a central bank meeting. It does not change the business days.', columns: ['Date', 'Diary entry type', 'Name', 'Description'], row: (e) => [`<span class="mono">${e.event_date}</span>`, esc(DIARY_TYPES[e.diary_entry_type] ?? e.diary_entry_type), esc(e.name), esc(e.description || '—')] },
};
function partRows(c, kind) { return (kind === 'rules' ? RULES : kind === 'exceptions' ? EXCEPTIONS : EVENTS)[c.code] ?? []; }

function partsPanel(c, kind) {
    const p = PART[kind];
    const rows = partRows(c, kind);
    const may = admin && shapeOf(c) !== 'quantlib';
    return `
      <p class="muted small">${esc(p.lead)}</p>
      <section class="card">
        <div class="table-wrap"><table>
          <thead><tr>${p.columns.map((h) => `<th>${h}</th>`).join('')}${may ? '<th></th>' : ''}</tr></thead>
          <tbody>${rows.length === 0 ? `<tr><td colspan="${p.columns.length + 1}"><div class="empty">None yet.</div></td></tr>` : rows.map((r) => `<tr>${p.row(r).map((v) => `<td>${v}</td>`).join('')}${may ? `<td class="actions-cell">
              <a class="btn sm ghost" href="${href({ dialog: 'part-history', part: r.id })}">${icon('history')} History</a>
              <a class="btn sm ghost" href="${href({ dialog: 'edit-part', part: r.id })}">${icon('edit')} Edit</a>
              <a class="btn sm ghost danger" href="${href({ dialog: 'remove-part', part: r.id })}">${icon('delete')} Delete</a></td>` : ''}</tr>`).join('')}</tbody>
        </table></div>
        ${may ? `<div class="under"><span class="muted small">${rows.length} ${p.title}${rows.length === 1 ? '' : 's'}</span><a class="btn primary" href="${href({ dialog: 'add-part' })}">${icon('add')} ${p.add}</a></div>` : ''}
      </section>`;
}

function daysPanel(c) {
    const year = state.year;
    const base = c.base_calendar_code ? find(c.base_calendar_code) : null;
    const other = state.compare ? find(state.compare) : null;
    const choices = ALL.filter((x) => x.code !== c.code);
    const picker = `<div class="field w-18"><label for="compare">Compare with</label><select id="compare" onchange="go({compare:this.value})"><option value="">—</option>${choices.map((x) => `<option value="${x.code}" ${x.code === state.compare ? 'selected' : ''}>${esc(x.name)}</option>`).join('')}</select></div>`;
    const bar = `<div class="days-bar">
        <div class="stepper"><a class="btn ghost sm" aria-label="Previous year" href="${href({ year: year - 1 })}">${icon('left')}</a><span class="year">${year}</span><a class="btn ghost sm" aria-label="Next year" href="${href({ year: year + 1 })}">${icon('right')}</a></div>
        ${picker}</div>`;
    if (state.built === 'none') {
        return `${bar}<div class="notice info">No business days are built for ${year}. ${admin ? `<a href="${href({ dialog: 'rebuild' })}">Rebuild the calendar</a> to build them.` : 'Ask a reference data administrator to rebuild the calendar.'}</div>`;
    }
    const mine = holidays(c, year);
    const theirs = other ? holidays(other, year) : new Map();
    const whereOf = (from) => (from === 'rule' ? 'Rule' : from === 'exception' || from === 'business' ? 'Exception' : 'Base');
    const entries = [];
    for (const [d, h] of mine) entries.push({ date: d, words: h.words, from: h.from, where: whereOf(h.from) });
    for (const [d, h] of theirs) if (!mine.has(d) && h.from !== 'business') entries.push({ date: d, words: `Holiday in ${other.code} only`, from: 'other', where: whereOf(h.from) });
    entries.sort((a, b) => a.date.localeCompare(b.date));
    const onWeekday = (e) => new Date(e.date + 'T00:00:00Z').getUTCDay() % 6 !== 0;
    const ownHolidays = entries.filter((e) => e.from !== 'business' && e.from !== 'other');
    const unionHolidays = entries.filter((e) => e.from !== 'business');
    const ownBusiness = weekdaysIn(year) - ownHolidays.filter(onWeekday).length;
    const unionBusiness = weekdaysIn(year) - unionHolidays.filter(onWeekday).length;
    const baseWithoutData = (shapeOf(c) === 'derived' || shapeOf(c) === 'quantlib') && inYearCount(c, year) === 0;
    const warn = baseWithoutData
        ? `<div class="notice warn">The prototype holds the QuantLib holidays of ${esc(c.base_calendar_code ?? c.code)} for 2026 only; ${year} shows the exceptions and nothing else.</div>`
        : base && shapeOf(base) === 'quantlib' && c.code === 'Acme.NewYork' ? `<div class="notice warn">${esc(base.code)} takes its holidays from QuantLib, but the rebuild does not read them yet. The days below show what the screen will show once it does.</div>` : '';
    return `${bar}${warn}
      <p class="muted small">${ownBusiness} business days and ${ownHolidays.length} weekday holidays in ${year}${other ? `. With ${esc(other.name)}: ${unionBusiness} business days and ${unionHolidays.length} holidays in the union, which is the definition a pair settles on` : ''}.</p>
      <div class="months">${MONTHS.map((m, i) => {
        const inMonth = entries.filter((e) => Number(e.date.slice(5, 7)) === i + 1);
        return `<div class="month"><h4>${m}</h4>${inMonth.length === 0 ? '<p class="small faint">No holidays.</p>' : `<ul>${inMonth.map((e) => `<li class="${e.from}"><span class="d">${e.date.slice(8)}</span><span class="w">${esc(e.words)}</span><span class="from">${esc(e.where)}</span></li>`).join('')}</ul>`}</div>`;
    }).join('')}</div>
      <p class="small faint">A day in green is a business day an exception opened. A day in italics is a holiday of the compared calendar only. A rebuild adds the days not built yet; a change to a rule or an exception shows once the year is rebuilt.</p>`;
}

/* The shared timeline: the event chart, then the rail of entries, newest first.
   Drawn here for one record; the same rail draws one part in its dialog. */
function timeline({ record, events, fields, canRevert, gaps }) {
    const hide = q.get('sys') === 'hide';
    const shown = hide ? events.filter((e) => !e.service) : events;
    const newest = events[0];
    const dayOf = (at) => at.slice(0, 10);
    const longDay = (d) => new Date(d + 'T00:00:00Z').toLocaleDateString('en-GB', { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' }).toUpperCase();
    const clock = (at) => at.slice(11, 19);
    const stamp = (at) => new Date(at).toLocaleString('en-GB', { day: 'numeric', month: 'short', year: 'numeric', hour: '2-digit', minute: '2-digit', timeZone: 'UTC' });

    // The chart: one bar per day from the first entry to the last, each split by kind.
    const days = [];
    if (shown.length) {
        const first = new Date(dayOf(shown[shown.length - 1].at) + 'T00:00:00Z');
        const last = new Date(dayOf(shown[0].at) + 'T00:00:00Z');
        for (let d = first; d <= last; d = addDays(d, 1)) days.push(iso(d));
    }
    const counts = Object.fromEntries(days.map((d) => [d, {}]));
    for (const e of shown) counts[dayOf(e.at)][e.kind] = (counts[dayOf(e.at)][e.kind] ?? 0) + 1;
    const max = Math.max(1, ...Object.values(counts).map((k) => Object.values(k).reduce((a, b) => a + b, 0)));
    const kinds = {};
    for (const e of shown) kinds[e.kind] = (kinds[e.kind] ?? 0) + 1;
    const chart = `<div class="chart card">
        <div class="bars" style="--max:${max}">${days.map((d) => { const k = counts[d]; const total = Object.values(k).reduce((a, b) => a + b, 0); return `<div class="bar" title="${d}: ${total} ${total === 1 ? 'entry' : 'entries'}" style="--n:${total}">${Object.entries(k).map(([kind, n]) => `<span style="flex:${n};background:${KIND_COLOUR[kind]}" title="${kind} ${n}"></span>`).join('')}</div>`; }).join('')}</div>
        <div class="chart-foot"><span>${days.length ? stamp(shown[shown.length - 1].at) : ''}</span><span>${days.length ? stamp(shown[0].at) : ''}</span></div>
        <div class="legend">${Object.entries(kinds).map(([k, n]) => `<span><i style="background:${KIND_COLOUR[k]}"></i>${k} ${n}</span>`).join('')}</div>
      </div>`;

    // The rail.
    let day = '';
    const rows = shown.map((e, i) => {
        const before = events.find((x) => x.version === e.version - 1);
        const current = e === newest;
        const divider = dayOf(e.at) !== day ? `<li class="day">${longDay(dayOf(e.at))}</li>` : '';
        day = dayOf(e.at);
        const changedNames = e.changes.map(([f]) => f).join(', ');
        const detail = e.changes.length > 0
            ? `<details><summary>Details</summary><table class="changes"><thead><tr><th>Field</th><th>Was</th><th>Now</th></tr></thead><tbody>${e.changes.map(([f, a, b]) => `<tr><td>${esc(f)}</td><td class="old">${esc(a)}</td><td class="new">${esc(b)}</td></tr>`).join('')}</tbody></table></details>`
            : e.kind === 'Changed' ? `<p class="small faint">No changes</p>`
            : `<details><summary>Details</summary><table class="changes"><tbody>${fields.map(([f, v]) => `<tr><td>${esc(f)}</td><td class="new">${esc(v)}</td></tr>`).join('')}</tbody></table></details>`;
        const revert = canRevert && !current ? `<div class="entry-actions"><a class="btn sm" href="${href({ dialog: 'revert', v: e.version })}">${icon('revert')} Revert</a></div>` : '';
        return `${divider}<li class="entry">
          <div class="gutter"><span class="mono">${clock(e.at)}</span>${current ? `<span class="current" role="img" aria-label="Current version" title="Current version">✓</span>` : ''}</div>
          <div class="rail"><span class="dot" style="border-color:${KIND_COLOUR[e.kind]}"></span></div>
          <div class="body">
            <div class="entry-head">
              <span class="avatar${e.service ? ' service' : ''}" title="${esc(e.by)}">${e.service ? '⚙' : esc(e.by.split('.').map((p) => p[0]).join('').toUpperCase())}</span>
              <span class="kind" style="border-color:${KIND_COLOUR[e.kind]};color:${KIND_COLOUR[e.kind]}">${e.kind}</span>
              <span class="mono muted">${esc(record)} v${e.version}</span>
              <span class="reason">${esc(e.reason)}</span>
              <a href="#" class="actor">${esc(e.by)}</a>
              ${changedNames ? `<span class="muted">${esc(changedNames)}</span>` : ''}
              <span class="when faint small">${stamp(e.at)}</span>
            </div>
            ${e.note ? `<p class="note">${esc(e.note)}</p>` : ''}
            ${detail}
            ${revert}
          </div>
        </li>`;
    }).join('');
    const gapBox = gaps.length ? `<div class="notice"><strong class="small">What this history cannot show</strong><ul class="small">${gaps.map((g) => `<li>${esc(g)}</li>`).join('')}</ul></div>` : '';
    return `<div class="timeline">
      <div class="tl-bar"><a class="btn ${hide ? '' : 'primary'}" href="${href({ sys: hide ? '' : 'hide' })}">${hide ? 'Show system activity' : 'Hide system activity'}</a></div>
      ${chart}
      <ol class="entries">${rows || '<li class="small muted">Every entry is system activity, and it is hidden.</li>'}</ol>
      ${gapBox}</div>`;
}

function historyPanel(c) {
    const events = HISTORY[c.code] ?? [{ ...SEED_EVENT, version: c.version }];
    const fields = [['Code', c.code], ['Name', c.name], ['Calendar type', CALENDAR_TYPES[c.calendar_type] ?? c.calendar_type], ['Country', c.country_code], ['Source', c.source], ['Editable', c.is_editable ? 'Yes' : 'No'], ['Base calendar', c.base_calendar_code ?? '—']];
    const gaps = shapeOf(c) === 'quantlib' ? ['The holidays QuantLib supplies are not versioned here, so a change to them does not appear.'] : [];
    return timeline({ record: `calendar ${c.code}`, events, fields, canRevert: admin && shapeOf(c) !== 'quantlib', gaps });
}

// ---------------------------------------------------------------------------
// Dialogs. Every write goes through one of the shared shapes: the record
// dialog, the remove dialog, the revert dialog, and the operation dialog.
// ---------------------------------------------------------------------------
function field(label, control, help, span) {
    return `<div class="field${span ? ' span' : ''}"><label>${esc(label)}</label>${control}${help ? `<span class="help">${esc(help)}</span>` : ''}</div>`;
}
function input(value, attrs = '') { return `<input type="text" value="${esc(value ?? '')}" ${attrs}>`; }
function select(options, value, attrs = '') { return `<select ${attrs}>${options.map(([v, l]) => `<option value="${esc(v)}" ${v === value ? 'selected' : ''}>${esc(l)}</option>`).join('')}</select>`; }
function reasonFields(firstIsTouch) {
    const options = REASONS.map((r) => [r, r]);
    return field('Why', select(options, firstIsTouch ? REASONS[0] : REASONS[1]), null, true) + field('Commentary', '<textarea rows="2"></textarea>', null, true);
}
function dialogShell({ title, loaded, body, foot, narrow }) {
    layer.innerHTML = `<div class="scrim" role="presentation"><div class="dialog${narrow ? ' narrow' : ''}" role="dialog" aria-modal="true" aria-labelledby="dlg-title">
      <div class="dialog-head"><div><h2 id="dlg-title">${title}</h2>${loaded ? `<span class="loaded">Loaded ${esc(loaded)}</span>` : ''}</div><a class="x closes" aria-label="Close" href="${href({ dialog: '', part: '', guard: '' })}">✕</a></div>
      <div class="dialog-body">${body}</div>
      <div class="dialog-foot">${foot}</div></div></div>`;
    guardChanges();
}
/* RS-WRITE-24: closing with changes asks first. The ✕ and Cancel alike close at once
   while nothing changed, and raise the guard once a field has. */
function guardChanges() {
    let dirty = false;
    layer.querySelectorAll('.dialog input, .dialog select, .dialog textarea').forEach((el) => el.addEventListener('input', () => { dirty = true; }));
    layer.querySelectorAll('.dialog a.closes').forEach((a) => a.addEventListener('click', (event) => {
        if (!dirty) return;
        event.preventDefault();
        go({ guard: '1' });
    }));
}
function cancelLink() { return `<a class="btn closes" href="${href({ dialog: '', part: '', guard: '' })}">${icon('cancel')} Cancel</a>`; }
function saveLink(label, iconName, done) { return `<a class="btn primary" href="${href({ dialog: '', part: '', done })}">${icon(iconName ?? 'save')} ${label}</a>`; }

function dialogCalendar(title, c, base) {
    const code = c ? c.code : '';
    dialogShell({
        title: esc(title),
        loaded: c ? LOADED : null,
        body: `<div class="form-grid">
          ${field('Code', input(code, c ? 'readonly' : 'placeholder="Acme.Paris"'), c ? 'Set when the record was made, and never changed here.' : 'Letters, digits and dots.')}
          ${field('Name', input(c ? c.name : ''))}
          ${field('Calendar type', select([['', '—'], ...Object.entries(CALENDAR_TYPES)], c ? c.calendar_type : (base ? base.calendar_type : '')))}
          ${field('Country', select([['', '—'], ...Object.entries(COUNTRIES)], c ? c.country_code : (base ? base.country_code : '')))}
          ${base ? field('Holidays from', input(`${base.code}, with exceptions`, 'readonly'), 'The new calendar takes its holidays from this one and adds exceptions.', true) : ''}
          ${field('Flag', `<span class="flag">${esc((c ? c.country_code : base ? base.country_code : 'ZZ').slice(0, 3))}</span>`, 'Optional. A picture for the list and the page.')}
          ${c ? reasonFields(true) : `<p class="small muted span">The row is recorded as a new record; no reason is needed.</p><p class="still span">Still to fill in: ${base ? 'Code, Name' : 'Code, Name, Calendar type, Country'}</p>`}
        </div>`,
        foot: `${cancelLink()}${saveLink(c ? 'Save' : 'Add', c ? 'save' : 'add', c ? 'saved' : 'added')}`,
    });
}

function dialogRebuild(c) {
    dialogShell({
        narrow: true,
        title: `Rebuild business days`,
        loaded: null,
        body: `<p class="muted small">Builds the days of <strong>${esc(c.name)}</strong> that are not built yet, from its ${shapeOf(c) === 'derived' ? 'base and its exceptions' : 'rules and its exceptions'}.</p>
          <div class="form-grid">${field('Build up to the end of', input(String(Math.max(state.year, 2027)), 'id="rebuild-to" inputmode="numeric" maxlength="4"'), 'A four-digit year.')}</div>
          <p class="small faint">A rebuild adds the days not built yet. It does not change a day that is already built, so a change to a rule or an exception does not show in a year already built.</p>`,
        foot: `${cancelLink()}<button class="btn primary" type="button" onclick="go({dialog:'', done:'rebuilt', built:'', to: document.getElementById('rebuild-to').value})">${icon('rebuild')} Rebuild</button>`,
    });
}

function partForm(c, kind, row) {
    if (kind === 'rules') {
        const k = row ? row.kind : 'fixed_date';
        const monthSel = select(MONTHS.map((m, i) => [String(i + 1), m]), String(row ? row.month : 1));
        const weekdaySel = select(WEEKDAYS.map((w, i) => [String(i), w]), String(row ? row.weekday : 1));
        return `<div class="form-grid">
          ${field('Kind', select([['fixed_date', 'Fixed date'], ['nth_weekday_of_month', 'Nth weekday of the month'], ['last_weekday_of_month', 'Last weekday of the month'], ['easter_offset', 'Easter offset']], k), 'The fields below follow the kind.', true)}
          ${k !== 'easter_offset' ? field('Month', monthSel) : ''}
          ${k === 'fixed_date' ? field('Day', input(row ? row.day : '', 'inputmode="numeric"'), '1 to 31.') : ''}
          ${k === 'nth_weekday_of_month' ? field('Occurrence', select([['1', '1st'], ['2', '2nd'], ['3', '3rd'], ['4', '4th']], String(row ? row.occurrence : 1))) : ''}
          ${k === 'nth_weekday_of_month' || k === 'last_weekday_of_month' ? field('Weekday', weekdaySel) : ''}
          ${k === 'easter_offset' ? field('Day offset', input(row ? row.day_offset : '', 'inputmode="numeric"'), 'Days from Easter Sunday; negative is before.') : ''}
          ${field('Shift', select([['none', 'None'], ['nearest_weekday', 'Nearest weekday'], ['roll_forward_to_monday', 'Roll forward to Monday']], row ? row.shift : 'none'), 'What happens when the date falls on a weekend.')}
          ${field('First year', input(row ? row.effective_from ?? '' : '', 'inputmode="numeric" maxlength="4"'), 'Optional. Empty means from the start.')}
          ${field('Last year', input(row ? row.effective_to ?? '' : '', 'inputmode="numeric" maxlength="4"'), 'Optional. Set it to retire a holiday from a year on.')}
          ${row ? reasonFields(true) : `<p class="small muted span">The row is recorded as a new record; no reason is needed.</p>`}
        </div>`;
    }
    if (kind === 'exceptions') {
        return `<div class="form-grid">
          ${field('Date', `<input type="date" value="${row ? row.exception_date : ''}">`)}
          ${field('Effect', select([['holiday', 'Holiday'], ['business', 'Business day']], row && row.is_business_day ? 'business' : 'holiday'), 'A holiday on a working day, or a business day on a holiday.')}
          ${field('Description', input(row ? row.description : ''), 'Optional. The words the Business days view shows for this date.', true)}
          ${row ? reasonFields(true) : `<p class="small muted span">The row is recorded as a new record; no reason is needed.</p>`}
        </div>`;
    }
    return `<div class="form-grid">
          ${field('Date', `<input type="date" value="${row ? row.event_date : ''}">`)}
          ${field('Diary entry type', select([['', '—'], ...Object.entries(DIARY_TYPES)], row ? row.diary_entry_type : ''))}
          ${field('Name', input(row ? row.name : ''), null, true)}
          ${field('Description', input(row ? row.description : ''), 'Optional.', true)}
          ${row ? reasonFields(true) : `<p class="small muted span">The row is recorded as a new record; no reason is needed.</p>`}
        </div>`;
}

function dialogs(c) {
    const tab = tabsOf(c).includes(state.tab) ? state.tab : 'details';
    const kind = ['rules', 'exceptions', 'events'].includes(tab) ? tab : '';
    const row = kind && state.part ? partRows(c, kind).find((r) => r.id === state.part) : null;
    const p = kind ? PART[kind] : null;
    if (state.done) { doneToast(); return; }
    if (state.guard) {
        layer.innerHTML = `<div class="guard"><div class="box"><h3>You have unsaved changes. Close anyway?</h3><div class="actions"><a class="btn" href="${href({ guard: '', dialog: state.dialog || 'edit' })}">Keep editing</a><a class="btn danger" href="${href({ guard: '', dialog: '', part: '' })}">Close anyway</a></div></div></div>`;
        return;
    }
    switch (state.dialog) {
        case 'edit': return dialogCalendar(`Edit ${esc(c.name)}`, c, null);
        case 'derive': return dialogCalendar(`Derive a calendar from ${esc(c.name)}`, null, c);
        case 'rebuild': return dialogRebuild(c);
        case 'delete': return dialogShell({ narrow: true, title: `Delete ${esc(c.name)}?`, body: `<div class="notice warn">This removes the calendar with its rules, exceptions and events. Currencies and pairs that use it keep their links.</div><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn danger" href="${href({ dialog: '', screen: 'list', code: '', tab: '', done: 'removed' })}">${icon('delete')} Delete</a>` });
        case 'add-part': return dialogShell({ title: esc(p.add), body: partForm(c, kind, null), foot: `${cancelLink()}${saveLink('Add', 'add', 'added')}` });
        case 'edit-part': return dialogShell({ title: `Edit ${esc(kind === 'rules' ? ruleWords(row) : kind === 'exceptions' ? row.exception_date : row.name)}`, loaded: LOADED, body: partForm(c, kind, row), foot: `${cancelLink()}${saveLink('Save', 'save', 'saved')}` });
        case 'remove-part': return dialogShell({ narrow: true, title: `Remove this ${esc(p.title)}?`, body: `<p class="muted small">${esc(kind === 'rules' ? ruleWords(row) : kind === 'exceptions' ? row.exception_date : row.name)}. The row closes. Its history stays.</p><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn danger" href="${href({ dialog: '', part: '', done: 'removed' })}">${icon('delete')} Remove</a>` });
        case 'part-history': {
            const name = kind === 'rules' ? ruleWords(row) : kind === 'exceptions' ? row.exception_date : row.name;
            const events = PART_HISTORY[row.id] ?? [{ version: 1, kind: 'Created', at: '2026-10-02T11:20:30Z', by: 'adrian.vance', service: false, reason: 'system.new_record', note: '', changes: [] }];
            const fields = Object.entries(row).filter(([k]) => !['id', 'version'].includes(k)).map(([k, v]) => [k.replace(/_/g, ' '), v === null || v === '' ? '—' : String(v)]);
            return dialogShell({ title: `History of ${esc(name)}`, body: timeline({ record: `${p.title} ${row.id}`, events, fields, canRevert: true, gaps: [] }), foot: `<a class="btn" href="${href({ dialog: '', part: '', sys: '' })}">Close</a>` });
        }
        case 'revert': return dialogShell({ narrow: true, title: `Revert to v${esc(q.get('v'))}?`, body: `<p class="muted small">A new version is written with the values of v${esc(q.get('v'))}. Nothing is deleted.</p><div class="form-grid">${reasonFields(false)}</div>`, foot: `${cancelLink()}<a class="btn primary" href="${href({ dialog: '', done: 'reverted', v: '' })}">${icon('revert')} Revert</a>` });
        default: layer.innerHTML = '';
    }
}

// ---------------------------------------------------------------------------
// The prototype bar
// ---------------------------------------------------------------------------
function bar() {
    document.getElementById('me').textContent = admin ? 'TA' : 'AV';
    document.getElementById('who-seg').innerHTML = [['admin', 'tenant_admin, who may write'], ['reader', 'a reader']].map(([v, l]) => `<a href="${href({ who: v, dialog: '' })}" aria-pressed="${state.who === v}">${l}</a>`).join('');
    document.getElementById('built-seg').innerHTML = [['yes', 'built'], ['none', 'not built yet']].map(([v, l]) => `<a href="${href({ built: v === 'yes' ? '' : v })}" aria-pressed="${state.built === v}">${l}</a>`).join('');
    document.getElementById('proto-where').innerHTML = `<a href="${href({ screen: 'list', code: '', tab: '', dialog: '', part: '', done: '', guard: '' })}">Calendars</a> · <a href="${href({ screen: 'calendar', code: 'Acme.Desk', tab: 'days', dialog: '' })}">Acme.Desk, business days</a> · <a href="${href({ screen: 'calendar', code: 'Acme.London', tab: 'details', dialog: '' })}">Acme.London, derived</a> · <a href="${href({ screen: 'calendar', code: 'TARGET', tab: 'details', dialog: '' })}">TARGET, QuantLib</a>`;
}

if (state.screen === 'calendar') renderCalendar(); else renderList();
bar();
