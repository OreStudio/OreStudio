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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP

#include "ores.refdata.api/domain/average_ois_convention.hpp"
#include "ores.refdata.api/domain/base_correlation_config.hpp"
#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
#include "ores.refdata.api/domain/bond_future_volatility_config.hpp"
#include "ores.refdata.api/domain/bond_yield_convention.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.api/domain/cds_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_volatility_term.hpp"
#include "ores.refdata.api/domain/cms_spread_option_convention.hpp"
#include "ores.refdata.api/domain/commodity_curve_config.hpp"
#include "ores.refdata.api/domain/commodity_forward_convention.hpp"
#include "ores.refdata.api/domain/commodity_future_convention.hpp"
#include "ores.refdata.api/domain/commodity_price_segment.hpp"
#include "ores.refdata.api/domain/commodity_volatility_config.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_convention.hpp"
#include "ores.refdata.api/domain/currency_pair.hpp"
#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include "ores.refdata.api/domain/curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/curve_configuration.hpp"
#include "ores.refdata.api/domain/curve_configuration_section.hpp"
#include "ores.refdata.api/domain/curve_correlation_config.hpp"
#include "ores.refdata.api/domain/curve_definition.hpp"
#include "ores.refdata.api/domain/curve_global_report.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter.hpp"
#include "ores.refdata.api/domain/curve_quote.hpp"
#include "ores.refdata.api/domain/curve_report_configuration.hpp"
#include "ores.refdata.api/domain/curve_security_config.hpp"
#include "ores.refdata.api/domain/curve_segment.hpp"
#include "ores.refdata.api/domain/curve_segment_curve.hpp"
#include "ores.refdata.api/domain/curve_volatility_config.hpp"
#include "ores.refdata.api/domain/default_curve_config.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.refdata.api/domain/deposit_convention.hpp"
#include "ores.refdata.api/domain/equity_curve_config.hpp"
#include "ores.refdata.api/domain/equity_volatility_config.hpp"
#include "ores.refdata.api/domain/fra_convention.hpp"
#include "ores.refdata.api/domain/future_convention.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.api/domain/fx_volatility_config.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/inflation_curve_config.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor.hpp"
#include "ores.refdata.api/domain/inflation_swap_convention.hpp"
#include "ores.refdata.api/domain/intraday_power_curve_config.hpp"
#include "ores.refdata.api/domain/intraday_power_load_convention.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.api/domain/swap_convention.hpp"
#include "ores.refdata.api/domain/swap_index_convention.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_two_swap_convention.hpp"
#include "ores.refdata.api/domain/yield_curve_config.hpp"
#include "ores.refdata.api/domain/yield_volatility_config.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.api/domain/zero_inflation_index_convention.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief An FX convention as a conventions document carries it: the pair, its
 * convention, and the advance calendars the document lists.
 */
struct fx_convention {
    ores::refdata::domain::currency_pair pair;
    ores::refdata::domain::currency_pair_convention convention;
    int spot_days = 0;
    std::vector<std::string> advance_calendars;
};

/**
 * @brief One ORE conventions document as the rows refdata stores.
 *
 * The instrument conventions belong to a party. The index and FX conventions
 * are world data, which every party in the tenant shares.
 */
struct conventions_document {
    std::vector<ores::refdata::domain::zero_convention> zero;
    std::vector<ores::refdata::domain::average_ois_convention> average_ois;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap;
    std::vector<ores::refdata::domain::cross_currency_basis_convention> cross_currency_basis;
    std::vector<ores::refdata::domain::cross_currency_fix_float_convention>
        cross_currency_fix_float;
    std::vector<ores::refdata::domain::tenor_basis_swap_convention> tenor_basis_swap;
    std::vector<ores::refdata::domain::tenor_basis_two_swap_convention> tenor_basis_two_swap;
    std::vector<ores::refdata::domain::deposit_convention> deposit;
    std::vector<ores::refdata::domain::swap_convention> swap;
    std::vector<ores::refdata::domain::swap_index_convention> swap_index;
    std::vector<ores::refdata::domain::future_convention> future;
    std::vector<ores::refdata::domain::fx_option_convention> fx_option;
    std::vector<ores::refdata::domain::inflation_swap_convention> inflation_swap;
    std::vector<ores::refdata::domain::intraday_power_load_convention> intraday_power_load;
    std::vector<ores::refdata::domain::ois_convention> ois;
    std::vector<ores::refdata::domain::fra_convention> fra;
    std::vector<ores::refdata::domain::ibor_index_convention> ibor_index;
    std::vector<ores::refdata::domain::overnight_index_convention> overnight_index;
    std::vector<ores::refdata::domain::zero_inflation_index_convention> zero_inflation_index;
    std::vector<fx_convention> fx;
    std::vector<ores::refdata::domain::cds_convention> cds;
    std::vector<ores::refdata::domain::cms_spread_option_convention> cms_spread_option;
    std::vector<ores::refdata::domain::commodity_future_convention> commodity_future;
    std::vector<ores::refdata::domain::commodity_forward_convention> commodity_forward;
    std::vector<ores::refdata::domain::bond_yield_convention> bond_yield;
};

/**
 * @brief One ORE curve configuration document as the rows refdata stores.
 *
 * The header row and every child row the document maps to, grouped by table.
 * Refdata stores and reads the document whole; a caller in another component
 * reaches it through these operations, never refdata's tables.
 */
struct curve_configuration_document {
    ores::refdata::domain::curve_configuration config;
    std::vector<ores::refdata::domain::curve_configuration_section> sections;
    std::vector<ores::refdata::domain::curve_definition> definitions;
    std::vector<ores::refdata::domain::yield_curve_config> yield_curves;
    std::vector<ores::refdata::domain::equity_curve_config> equity_curves;
    std::vector<ores::refdata::domain::inflation_curve_config> inflation_curves;
    std::vector<ores::refdata::domain::default_curve_config> default_curves;
    std::vector<ores::refdata::domain::commodity_curve_config> commodity_curves;
    std::vector<ores::refdata::domain::fx_volatility_config> fx_volatilities;
    std::vector<ores::refdata::domain::yield_volatility_config> yield_volatilities;
    std::vector<ores::refdata::domain::base_correlation_config> base_correlations;
    std::vector<ores::refdata::domain::curve_correlation_config> correlations;
    std::vector<ores::refdata::domain::curve_report_configuration> report_configurations;
    std::vector<ores::refdata::domain::cds_volatility_config> cds_volatilities;
    std::vector<ores::refdata::domain::cds_volatility_term> cds_volatility_terms;
    std::vector<ores::refdata::domain::curve_volatility_config> volatility_configs;
    std::vector<ores::refdata::domain::inflation_cap_floor_volatility_config>
        inflation_cap_floor_volatilities;
    std::vector<ores::refdata::domain::swaption_volatility_config> swaption_volatilities;
    std::vector<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatilities;
    std::vector<ores::refdata::domain::curve_parametric_smile> parametric_smiles;
    std::vector<ores::refdata::domain::curve_parametric_smile_parameter>
        parametric_smile_parameters;
    std::vector<ores::refdata::domain::equity_volatility_config> equity_volatilities;
    std::vector<ores::refdata::domain::commodity_volatility_config> commodity_volatilities;
    std::vector<ores::refdata::domain::bond_future_volatility_config> bond_future_volatilities;
    std::vector<ores::refdata::domain::curve_global_report> global_reports;
    std::vector<ores::refdata::domain::commodity_price_segment> commodity_price_segments;
    std::vector<ores::refdata::domain::default_curve_configuration> default_curve_configurations;
    std::vector<ores::refdata::domain::inflation_seasonality_factor> seasonality_factors;
    std::vector<ores::refdata::domain::curve_security_config> securities;
    std::vector<ores::refdata::domain::intraday_power_curve_config> intraday_power_curves;
    std::vector<ores::refdata::domain::curve_bootstrap_config> bootstrap_configs;
    std::vector<ores::refdata::domain::curve_segment> segments;
    std::vector<ores::refdata::domain::curve_segment_curve> segment_curves;
    std::vector<ores::refdata::domain::curve_quote> quotes;
};

/**
 * @brief Stores a curve configuration document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_curve_configuration_document_request {
    using response_type = struct save_curve_configuration_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_configuration_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_configuration_document document;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_curve_configuration_document_response {
    bool success = false;
    std::string message;
    std::string id;
};

/**
 * @brief Reads a curve configuration document by the reporting configuration it fills.
 */
struct get_curve_configuration_document_request {
    using response_type = struct get_curve_configuration_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_configuration_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;
};

/**
 * @brief A curve configuration document, when one fills the configuration.
 */
struct get_curve_configuration_document_response {
    bool success = false;
    std::string message;
    curve_configuration_document document;
};

/**
 * @brief Deletes a curve configuration document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
struct delete_curve_configuration_document_request {
    using response_type = struct delete_curve_configuration_document_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_configuration_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_curve_configuration_document_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Stores a conventions document.
 *
 * The instrument conventions belong to the session's party and replace any
 * it holds under the same id. A world convention the tenant lacks is added;
 * one it holds is left as it is.
 */
struct save_conventions_document_request {
    using response_type = struct save_conventions_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_document document;
};

/**
 * @brief What the save did not store, and why.
 */
struct save_conventions_document_response {
    bool success = false;
    std::string message;
    /** World conventions the tenant already held, left unchanged. */
    std::vector<std::string> world_kept;
    /** FX conventions, by ORE id, which have no store yet. */
    std::vector<std::string> fx_skipped;
};

/**
 * @brief Reads every convention the party sees: its instrument conventions
 * and the tenant's world conventions.
 */
struct get_conventions_document_request {
    using response_type = struct get_conventions_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;
};

/**
 * @brief The conventions the party sees.
 */
struct get_conventions_document_response {
    bool success = false;
    std::string message;
    conventions_document document;
};

}

#endif
