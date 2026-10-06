/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 */
#include "ores.refdata.service/messaging/event_registrar.hpp"

// Per-entity generated event-mapping registrars.
//
// Every ores.refdata domain_entity that generates a registrar is composed
// here, so a change to any of them reaches the caches and the audit trail on
// that entity's own subjects. This is the one composition point. application.cpp
// registers no entity mapping and subscribes to no changed event of its own.
#include "ores.refdata.service/messaging/asset_class_code_event_registrar.hpp"
#include "ores.refdata.service/messaging/average_ois_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/base_correlation_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/bma_basis_swap_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/bond_future_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/bond_yield_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_purpose_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_status_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_centre_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_day_convention_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_unit_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_unit_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_event_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_exception_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_name_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_rule_event_registrar.hpp"
#include "ores.refdata.service/messaging/calendar_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/cap_floor_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/cds_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/cds_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/cds_volatility_term_event_registrar.hpp"
#include "ores.refdata.service/messaging/cms_spread_option_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/commodity_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/commodity_forward_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/commodity_future_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/commodity_price_segment_event_registrar.hpp"
#include "ores.refdata.service/messaging/commodity_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/contact_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/counterparty_contact_information_event_registrar.hpp"
#include "ores.refdata.service/messaging/counterparty_event_registrar.hpp"
#include "ores.refdata.service/messaging/counterparty_identifier_event_registrar.hpp"
#include "ores.refdata.service/messaging/country_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_driver_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_enabled_derived_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_topology_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/cross_currency_basis_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/cross_currency_fix_float_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/csa_eligible_currency_event_registrar.hpp"
#include "ores.refdata.service/messaging/csa_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_group_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_market_tier_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_pair_classification_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_pair_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_bootstrap_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_configuration_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_configuration_section_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_correlation_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_definition_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_global_report_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_parametric_smile_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_parametric_smile_parameter_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_quote_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_report_configuration_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_role_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_section_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_security_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_segment_curve_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_segment_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_segment_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/day_count_fraction_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/day_counter_event_registrar.hpp"
#include "ores.refdata.service/messaging/default_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/default_curve_configuration_event_registrar.hpp"
#include "ores.refdata.service/messaging/deposit_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/derivation_kind_event_registrar.hpp"
#include "ores.refdata.service/messaging/diary_entry_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/equity_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/equity_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/floating_index_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/fra_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/future_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/fx_option_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/fx_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/ibor_index_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/inflation_cap_floor_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/inflation_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/inflation_seasonality_factor_event_registrar.hpp"
#include "ores.refdata.service/messaging/inflation_swap_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/instrument_code_event_registrar.hpp"
#include "ores.refdata.service/messaging/intraday_power_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/intraday_power_load_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/ir_curve_bootstrap_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/ir_curve_bootstrap_pillar_event_registrar.hpp"
#include "ores.refdata.service/messaging/ledger_feed_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/leg_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/monetary_nature_event_registrar.hpp"
#include "ores.refdata.service/messaging/netting_agreement_event_registrar.hpp"
#include "ores.refdata.service/messaging/netting_set_event_registrar.hpp"
#include "ores.refdata.service/messaging/netting_set_identifier_event_registrar.hpp"
#include "ores.refdata.service/messaging/ois_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/overnight_index_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_contact_information_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_id_scheme_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_identifier_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_status_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/payment_frequency_event_registrar.hpp"
#include "ores.refdata.service/messaging/portfolio_event_registrar.hpp"
#include "ores.refdata.service/messaging/portfolio_right_event_registrar.hpp"
#include "ores.refdata.service/messaging/purpose_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/regulatory_book_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/rounding_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/sandbox_event_registrar.hpp"
#include "ores.refdata.service/messaging/sandbox_member_event_registrar.hpp"
#include "ores.refdata.service/messaging/series_subclass_code_event_registrar.hpp"
#include "ores.refdata.service/messaging/swap_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/swap_index_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/swaption_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_anchor_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_basis_swap_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_basis_two_swap_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_kind_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_resolution_algorithm_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_schedule_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_unit_event_registrar.hpp"
#include "ores.refdata.service/messaging/yield_curve_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/yield_volatility_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/zero_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/zero_inflation_index_convention_event_registrar.hpp"

namespace ores::refdata::service::messaging {

std::vector<ores::eventing::service::subscription> event_registrar::register_event_mappings(
    ores::eventing::service::postgres_event_source& event_source,
    ores::eventing::service::event_bus& event_bus,
    ores::nats::service::client& nats) {
    std::vector<ores::eventing::service::subscription> subs;

    // ----------------------------------------------------------------
    // Per-entity event mappings. Each register_<entity>_event_mapping()
    // registers the entity's Postgres NOTIFY channel and returns the
    // event_bus subscription that republishes it to NATS; we take
    // ownership of the subscriptions here so they outlive this call.
    // ----------------------------------------------------------------
    subs.push_back(register_asset_class_code_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_purpose_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_status_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_business_day_convention_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_business_unit_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_business_unit_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_contact_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_counterparty_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_country_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_driver_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_enabled_derived_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_topology_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_market_tier_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_pair_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_day_count_fraction_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_diary_entry_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_floating_index_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_instrument_code_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ledger_feed_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_leg_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_monetary_nature_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_id_scheme_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_payment_frequency_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_purpose_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_portfolio_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_regulatory_book_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_rounding_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_series_subclass_code_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_anchor_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_kind_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_role_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_unit_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_tenor_resolution_algorithm_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_schedule_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_average_ois_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_base_correlation_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_bma_basis_swap_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_bond_future_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_bond_yield_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_business_centre_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_event_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_exception_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_name_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_rule_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_calendar_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cap_floor_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cds_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cds_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cds_volatility_term_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cms_spread_option_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_commodity_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_commodity_forward_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_commodity_future_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_commodity_price_segment_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_commodity_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_counterparty_contact_information_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_counterparty_identifier_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cross_currency_basis_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_cross_currency_fix_float_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_csa_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_csa_eligible_currency_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_group_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_pair_classification_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_bootstrap_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_configuration_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_configuration_section_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_correlation_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_definition_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_global_report_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_parametric_smile_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_parametric_smile_parameter_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_quote_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_report_configuration_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_section_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_security_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_segment_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_segment_curve_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_segment_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_day_counter_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_default_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_default_curve_configuration_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_deposit_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_derivation_kind_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_equity_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_equity_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_fra_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_future_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_fx_option_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_fx_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ibor_index_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_inflation_cap_floor_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_inflation_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_inflation_seasonality_factor_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_inflation_swap_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_intraday_power_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_intraday_power_load_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ir_curve_bootstrap_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ir_curve_bootstrap_pillar_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_netting_agreement_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_netting_set_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_netting_set_identifier_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ois_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_overnight_index_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_contact_information_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_identifier_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_status_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_portfolio_right_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_sandbox_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_sandbox_member_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_swap_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_swap_index_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_swaption_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_basis_swap_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_basis_two_swap_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_yield_curve_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_yield_volatility_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_zero_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_zero_inflation_index_convention_event_mapping(event_source, event_bus, nats));

    return subs;
}

}
