/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import type { EntityRouteDescriptor } from './entity-routes.js';
import { assetClassCodeRoute } from './generated/refdata/asset_class_code_route.js';
import { bookPurposeTypeRoute } from './generated/refdata/book_purpose_type_route.js';
import { bookRoute } from './generated/refdata/book_route.js';
import { bookStatusRoute } from './generated/refdata/book_status_route.js';
import { businessCentreRoute } from './generated/refdata/business_centre_route.js';
import { businessDayConventionTypeRoute } from './generated/refdata/business_day_convention_type_route.js';
import { businessUnitRoute } from './generated/refdata/business_unit_route.js';
import { businessUnitTypeRoute } from './generated/refdata/business_unit_type_route.js';
import { calendarDateRoute } from './generated/refdata/calendar_date_route.js';
import { calendarEventRoute } from './generated/refdata/calendar_event_route.js';
import { calendarExceptionRoute } from './generated/refdata/calendar_exception_route.js';
import { calendarRoute } from './generated/refdata/calendar_route.js';
import { calendarRuleRoute } from './generated/refdata/calendar_rule_route.js';
import { calendarTypeRoute } from './generated/refdata/calendar_type_route.js';
import { cdsConventionRoute } from './generated/refdata/cds_convention_route.js';
import { contactTypeRoute } from './generated/refdata/contact_type_route.js';
import { counterpartyContactInformationRoute } from './generated/refdata/counterparty_contact_information_route.js';
import { counterpartyIdentifierRoute } from './generated/refdata/counterparty_identifier_route.js';
import { counterpartyRoute } from './generated/refdata/counterparty_route.js';
import { countryRoute } from './generated/refdata/country_route.js';
import { crmDriverPairRoute } from './generated/refdata/crm_driver_pair_route.js';
import { crmEnabledDerivedPairRoute } from './generated/refdata/crm_enabled_derived_pair_route.js';
import { crmTopologyConfigRoute } from './generated/refdata/crm_topology_config_route.js';
import { currencyGroupRoute } from './generated/refdata/currency_group_route.js';
import { currencyMarketTierRoute } from './generated/refdata/currency_market_tier_route.js';
import { currencyPairClassificationRoute } from './generated/refdata/currency_pair_classification_route.js';
import { currencyPairConventionRoute } from './generated/refdata/currency_pair_convention_route.js';
import { currencyPairRoute } from './generated/refdata/currency_pair_route.js';
import { currencyRoute } from './generated/refdata/currency_route.js';
import { curveRoleRoute } from './generated/refdata/curve_role_route.js';
import { dayCountFractionTypeRoute } from './generated/refdata/day_count_fraction_type_route.js';
import { depositConventionRoute } from './generated/refdata/deposit_convention_route.js';
import { diaryEntryTypeRoute } from './generated/refdata/diary_entry_type_route.js';
import { floatingIndexTypeRoute } from './generated/refdata/floating_index_type_route.js';
import { fraConventionRoute } from './generated/refdata/fra_convention_route.js';
import { iborIndexConventionRoute } from './generated/refdata/ibor_index_convention_route.js';
import { instrumentCodeRoute } from './generated/refdata/instrument_code_route.js';
import { irCurveBootstrapConfigRoute } from './generated/refdata/ir_curve_bootstrap_config_route.js';
import { ledgerFeedTypeRoute } from './generated/refdata/ledger_feed_type_route.js';
import { legTypeRoute } from './generated/refdata/leg_type_route.js';
import { monetaryNatureRoute } from './generated/refdata/monetary_nature_route.js';
import { oisConventionRoute } from './generated/refdata/ois_convention_route.js';
import { overnightIndexConventionRoute } from './generated/refdata/overnight_index_convention_route.js';
import { partyContactInformationRoute } from './generated/refdata/party_contact_information_route.js';
import { partyIdSchemeRoute } from './generated/refdata/party_id_scheme_route.js';
import { partyIdentifierRoute } from './generated/refdata/party_identifier_route.js';
import { partyRoute } from './generated/refdata/party_route.js';
import { partyStatusRoute } from './generated/refdata/party_status_route.js';
import { partyTypeRoute } from './generated/refdata/party_type_route.js';
import { paymentFrequencyRoute } from './generated/refdata/payment_frequency_route.js';
import { portfolioRoute } from './generated/refdata/portfolio_route.js';
import { purposeTypeRoute } from './generated/refdata/purpose_type_route.js';
import { regulatoryBookTypeRoute } from './generated/refdata/regulatory_book_type_route.js';
import { roundingTypeRoute } from './generated/refdata/rounding_type_route.js';
import { seriesSubclassCodeRoute } from './generated/refdata/series_subclass_code_route.js';
import { swapConventionRoute } from './generated/refdata/swap_convention_route.js';
import { tenorAnchorRoute } from './generated/refdata/tenor_anchor_route.js';
import { tenorConventionRoute } from './generated/refdata/tenor_convention_route.js';
import { tenorKindRoute } from './generated/refdata/tenor_kind_route.js';
import { tenorResolutionAlgorithmRoute } from './generated/refdata/tenor_resolution_algorithm_route.js';
import { tenorRoute } from './generated/refdata/tenor_route.js';
import { tenorScheduleRoute } from './generated/refdata/tenor_schedule_route.js';
import { tenorUnitRoute } from './generated/refdata/tenor_unit_route.js';
import { zeroConventionRoute } from './generated/refdata/zero_convention_route.js';
import { accountContactInformationRoute } from './generated/iam/account_contact_information_route.js';
import { accountTypeRoute } from './generated/iam/account_type_route.js';
import { tenantRoute } from './generated/iam/tenant_route.js';
import { tenantStatusRoute } from './generated/iam/tenant_status_route.js';
import { tenantTypeRoute } from './generated/iam/tenant_type_route.js';

/**
 * Every entity route the models declare.
 *
 * The BFF serves what the models declare and nothing else, so the list is the
 * set of generated descriptors. It is stated here rather than discovered at
 * run time because a route that failed to load would be a screen that 404s at
 * the first request instead of a build that fails, and because the server
 * bundling step already resolves these imports.
 */
export const entityRoutes: readonly EntityRouteDescriptor[] = [
  assetClassCodeRoute,
  bookPurposeTypeRoute,
  bookRoute,
  bookStatusRoute,
  businessCentreRoute,
  businessDayConventionTypeRoute,
  businessUnitRoute,
  businessUnitTypeRoute,
  calendarDateRoute,
  calendarEventRoute,
  calendarExceptionRoute,
  calendarRoute,
  calendarRuleRoute,
  calendarTypeRoute,
  cdsConventionRoute,
  contactTypeRoute,
  counterpartyContactInformationRoute,
  counterpartyIdentifierRoute,
  counterpartyRoute,
  countryRoute,
  crmDriverPairRoute,
  crmEnabledDerivedPairRoute,
  crmTopologyConfigRoute,
  currencyGroupRoute,
  currencyMarketTierRoute,
  currencyPairClassificationRoute,
  currencyPairConventionRoute,
  currencyPairRoute,
  currencyRoute,
  curveRoleRoute,
  dayCountFractionTypeRoute,
  depositConventionRoute,
  diaryEntryTypeRoute,
  floatingIndexTypeRoute,
  fraConventionRoute,
  iborIndexConventionRoute,
  instrumentCodeRoute,
  irCurveBootstrapConfigRoute,
  ledgerFeedTypeRoute,
  legTypeRoute,
  monetaryNatureRoute,
  oisConventionRoute,
  overnightIndexConventionRoute,
  partyContactInformationRoute,
  partyIdSchemeRoute,
  partyIdentifierRoute,
  partyRoute,
  partyStatusRoute,
  partyTypeRoute,
  paymentFrequencyRoute,
  portfolioRoute,
  purposeTypeRoute,
  regulatoryBookTypeRoute,
  roundingTypeRoute,
  seriesSubclassCodeRoute,
  swapConventionRoute,
  tenorAnchorRoute,
  tenorConventionRoute,
  tenorKindRoute,
  tenorResolutionAlgorithmRoute,
  tenorRoute,
  tenorScheduleRoute,
  tenorUnitRoute,
  zeroConventionRoute,
  accountContactInformationRoute,
  accountTypeRoute,
  tenantRoute,
  tenantStatusRoute,
  tenantTypeRoute,
];
