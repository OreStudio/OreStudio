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
#ifndef ORES_REFDATA_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP

#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::refdata::messaging {

/**
 * @brief The workflow step that publishes the DQ-cleared trading account types.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_trading_account_types_from_dq_fn.
 */
struct publish_trading_account_types_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_trading_account_types_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_trading_account_types_from_dq_request&,
                           const publish_trading_account_types_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared asset classes.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_asset_classes_from_dq_fn.
 */
struct publish_asset_classes_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_asset_classes_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_asset_classes_from_dq_request&,
                           const publish_asset_classes_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared asset measures.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_asset_measures_from_dq_fn.
 */
struct publish_asset_measures_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_asset_measures_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_asset_measures_from_dq_request&,
                           const publish_asset_measures_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared benchmark rates.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_benchmark_rates_from_dq_fn.
 */
struct publish_benchmark_rates_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_benchmark_rates_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_benchmark_rates_from_dq_request&,
                           const publish_benchmark_rates_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared books.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_books_from_dq_fn.
 */
struct publish_books_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_books_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_books_from_dq_request&,
                           const publish_books_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared business centres.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_business_centres_from_dq_fn.
 */
struct publish_business_centres_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_business_centres_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_business_centres_from_dq_request&,
                           const publish_business_centres_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared business processes.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_business_processes_from_dq_fn.
 */
struct publish_business_processes_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_business_processes_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_business_processes_from_dq_request&,
                           const publish_business_processes_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared business units.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_business_units_from_dq_fn.
 */
struct publish_business_units_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_business_units_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_business_units_from_dq_request&,
                           const publish_business_units_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared calendar events.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_calendar_events_from_dq_fn.
 */
struct publish_calendar_events_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_calendar_events_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_calendar_events_from_dq_request&,
                           const publish_calendar_events_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared calendar exceptions.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_calendar_exceptions_from_dq_fn.
 */
struct publish_calendar_exceptions_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_calendar_exceptions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_calendar_exceptions_from_dq_request&,
                           const publish_calendar_exceptions_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared calendar types.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_calendar_types_from_dq_fn.
 */
struct publish_calendar_types_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_calendar_types_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_calendar_types_from_dq_request&,
                           const publish_calendar_types_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared calendars.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_calendars_from_dq_fn.
 */
struct publish_calendars_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_calendars_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_calendars_from_dq_request&,
                           const publish_calendars_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared cashflow types.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_cashflow_types_from_dq_fn.
 */
struct publish_cashflow_types_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_cashflow_types_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_cashflow_types_from_dq_request&,
                           const publish_cashflow_types_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared commodities.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_commodities_from_dq_fn.
 */
struct publish_commodities_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_commodities_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_commodities_from_dq_request&,
                           const publish_commodities_from_dq_request&) = default;
};

/**
 * @brief Publishes the legal entities a tenant may reference.
 */
struct publish_legal_entities_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_legal_entities_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_legal_entities_from_dq_request&,
                           const publish_legal_entities_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared equity underlyings.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_equity_underlyings_from_dq_fn, which the handler derives
 * from the subject's last segment.
 */
struct publish_equity_underlyings_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_equity_underlyings_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_equity_underlyings_from_dq_request&,
                           const publish_equity_underlyings_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared conventions.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_conventions_from_dq_fn.
 */
struct publish_conventions_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_conventions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_conventions_from_dq_request&,
                           const publish_conventions_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared counterparty aliases.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_counterparty_aliases_from_dq_fn.
 */
struct publish_counterparty_aliases_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_counterparty_aliases_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_counterparty_aliases_from_dq_request&,
                           const publish_counterparty_aliases_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared countries.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_countries_from_dq_fn.
 */
struct publish_countries_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_countries_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_countries_from_dq_request&,
                           const publish_countries_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared crm topology bundles.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_crm_topology_bundles_from_dq_fn.
 */
struct publish_crm_topology_bundles_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_crm_topology_bundles_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_crm_topology_bundles_from_dq_request&,
                           const publish_crm_topology_bundles_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared csas.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_csas_from_dq_fn.
 */
struct publish_csas_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_csas_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_csas_from_dq_request&,
                           const publish_csas_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currencies.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currencies_from_dq_fn.
 */
struct publish_currencies_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_currencies_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currencies_from_dq_request&,
                           const publish_currencies_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared curve configuration.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_curve_configuration_from_dq_fn.
 */
struct publish_curve_configuration_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_curve_configuration_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_curve_configuration_from_dq_request&,
                           const publish_curve_configuration_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency groups.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_groups_from_dq_fn.
 */
struct publish_currency_groups_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_groups_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_groups_from_dq_request&,
                           const publish_currency_groups_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency currency groups.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_currency_groups_from_dq_fn.
 */
struct publish_currency_currency_groups_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_currency_groups_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_currency_groups_from_dq_request&,
                           const publish_currency_currency_groups_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency pairs.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_pairs_from_dq_fn.
 */
struct publish_currency_pairs_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_pairs_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_pairs_from_dq_request&,
                           const publish_currency_pairs_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency pair conventions.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_pair_conventions_from_dq_fn.
 */
struct publish_currency_pair_conventions_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_pair_conventions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_pair_conventions_from_dq_request&,
                           const publish_currency_pair_conventions_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency calendars.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_calendars_from_dq_fn.
 */
struct publish_currency_calendars_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_calendars_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_calendars_from_dq_request&,
                           const publish_currency_calendars_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency countries.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_countries_from_dq_fn.
 */
struct publish_currency_countries_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_countries_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_currency_countries_from_dq_request&,
                           const publish_currency_countries_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared currency pair convention calendars.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_currency_pair_convention_calendars_from_dq_fn.
 */
struct publish_currency_pair_convention_calendars_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_currency_pair_convention_calendars_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool
    operator==(const publish_currency_pair_convention_calendars_from_dq_request&,
               const publish_currency_pair_convention_calendars_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared entity classifications.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_entity_classifications_from_dq_fn.
 */
struct publish_entity_classifications_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_entity_classifications_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_entity_classifications_from_dq_request&,
                           const publish_entity_classifications_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared lei counterparties.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_lei_counterparties_from_dq_fn.
 */
struct publish_lei_counterparties_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_lei_counterparties_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_lei_counterparties_from_dq_request&,
                           const publish_lei_counterparties_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared lei parties.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_lei_parties_from_dq_fn.
 */
struct publish_lei_parties_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_lei_parties_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_lei_parties_from_dq_request&,
                           const publish_lei_parties_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared local jurisdictions.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_local_jurisdictions_from_dq_fn.
 */
struct publish_local_jurisdictions_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_local_jurisdictions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_local_jurisdictions_from_dq_request&,
                           const publish_local_jurisdictions_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared sandbox portfolios.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_sandbox_portfolios_from_dq_fn.
 */
struct publish_sandbox_portfolios_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_sandbox_portfolios_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_sandbox_portfolios_from_dq_request&,
                           const publish_sandbox_portfolios_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared netting agreements.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_netting_agreements_from_dq_fn.
 */
struct publish_netting_agreements_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_netting_agreements_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_netting_agreements_from_dq_request&,
                           const publish_netting_agreements_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared netting set aliases.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_netting_set_aliases_from_dq_fn.
 */
struct publish_netting_set_aliases_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_netting_set_aliases_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_netting_set_aliases_from_dq_request&,
                           const publish_netting_set_aliases_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared netting sets.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_netting_sets_from_dq_fn.
 */
struct publish_netting_sets_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_netting_sets_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_netting_sets_from_dq_request&,
                           const publish_netting_sets_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party contact informations.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_contact_informations_from_dq_fn.
 */
struct publish_party_contact_informations_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_party_contact_informations_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_contact_informations_from_dq_request&,
                           const publish_party_contact_informations_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party counterparties.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_counterparties_from_dq_fn.
 */
struct publish_party_counterparties_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_party_counterparties_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_counterparties_from_dq_request&,
                           const publish_party_counterparties_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party countries.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_countries_from_dq_fn.
 */
struct publish_party_countries_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_party_countries_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_countries_from_dq_request&,
                           const publish_party_countries_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party currencies.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_currencies_from_dq_fn.
 */
struct publish_party_currencies_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_party_currencies_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_currencies_from_dq_request&,
                           const publish_party_currencies_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party relationships.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_relationships_from_dq_fn.
 */
struct publish_party_relationships_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_party_relationships_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_relationships_from_dq_request&,
                           const publish_party_relationships_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared party roles.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_party_roles_from_dq_fn.
 */
struct publish_party_roles_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_party_roles_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_party_roles_from_dq_request&,
                           const publish_party_roles_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared payment frequencies.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_payment_frequencies_from_dq_fn.
 */
struct publish_payment_frequencies_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_payment_frequencies_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_payment_frequencies_from_dq_request&,
                           const publish_payment_frequencies_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared person roles.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_person_roles_from_dq_fn.
 */
struct publish_person_roles_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_person_roles_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_person_roles_from_dq_request&,
                           const publish_person_roles_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared portfolios.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_portfolios_from_dq_fn.
 */
struct publish_portfolios_from_dq_request {
    static constexpr std::string_view nats_subject = "refdata.v1.ops.publish_portfolios_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_portfolios_from_dq_request&,
                           const publish_portfolios_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared regulatory corporate sectors.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_regulatory_corporate_sectors_from_dq_fn.
 */
struct publish_regulatory_corporate_sectors_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_regulatory_corporate_sectors_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_regulatory_corporate_sectors_from_dq_request&,
                           const publish_regulatory_corporate_sectors_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared reporting regimes.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_reporting_regimes_from_dq_fn.
 */
struct publish_reporting_regimes_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_reporting_regimes_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_reporting_regimes_from_dq_request&,
                           const publish_reporting_regimes_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared supervisory bodies.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_supervisory_bodies_from_dq_fn.
 */
struct publish_supervisory_bodies_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_supervisory_bodies_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_supervisory_bodies_from_dq_request&,
                           const publish_supervisory_bodies_from_dq_request&) = default;
};

/**
 * @brief The workflow step that publishes the DQ-cleared tenor schedules.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_refdata_publish_tenor_schedules_from_dq_fn.
 */
struct publish_tenor_schedules_from_dq_request {
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.publish_tenor_schedules_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_tenor_schedules_from_dq_request&,
                           const publish_tenor_schedules_from_dq_request&) = default;
};

/**
 * @brief The members of publish_trading_account_types_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_trading_account_types_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_asset_classes_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_asset_classes_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_asset_measures_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_asset_measures_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_benchmark_rates_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_benchmark_rates_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_books_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_books_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_business_centres_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_business_centres_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_business_processes_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_business_processes_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_business_units_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_business_units_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_calendar_events_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_calendar_events_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_calendar_exceptions_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_calendar_exceptions_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_calendar_types_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_calendar_types_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_calendars_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_calendars_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_cashflow_types_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_cashflow_types_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_commodities_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_commodities_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_legal_entities_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_legal_entities_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_equity_underlyings_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_equity_underlyings_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_conventions_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_conventions_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_counterparty_aliases_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_counterparty_aliases_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_countries_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_countries_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_crm_topology_bundles_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_crm_topology_bundles_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_csas_from_dq_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_csas_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currencies_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_currencies_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_curve_configuration_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_curve_configuration_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_groups_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_groups_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_currency_groups_from_dq_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_currency_groups_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_pairs_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_pairs_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_pair_conventions_from_dq_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_pair_conventions_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_calendars_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_calendars_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_countries_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_countries_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_currency_pair_convention_calendars_from_dq_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_currency_pair_convention_calendars_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_entity_classifications_from_dq_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_entity_classifications_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_lei_counterparties_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_lei_counterparties_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_lei_parties_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_lei_parties_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_local_jurisdictions_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_local_jurisdictions_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_sandbox_portfolios_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_sandbox_portfolios_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_netting_agreements_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_netting_agreements_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_netting_set_aliases_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_netting_set_aliases_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_netting_sets_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_netting_sets_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_contact_informations_from_dq_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_party_contact_informations_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_counterparties_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_party_counterparties_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_countries_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_party_countries_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_currencies_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_party_currencies_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_relationships_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_party_relationships_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_party_roles_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_party_roles_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_payment_frequencies_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_payment_frequencies_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_person_roles_from_dq_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_person_roles_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_portfolios_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_portfolios_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_regulatory_corporate_sectors_from_dq_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_regulatory_corporate_sectors_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_reporting_regimes_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_reporting_regimes_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_supervisory_bodies_from_dq_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_supervisory_bodies_from_dq_request>) {
    return std::tuple{};
}

/**
 * @brief The members of publish_tenor_schedules_from_dq_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_tenor_schedules_from_dq_request>) {
    return std::tuple{};
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_trading_account_types_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_trading_account_types_from_dq_request& v);
    static ores::refdata::messaging::publish_trading_account_types_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_trading_account_types_from_dq_request& v);
    static ores::refdata::messaging::publish_trading_account_types_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_asset_classes_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_asset_classes_from_dq_request& v);
    static ores::refdata::messaging::publish_asset_classes_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_asset_classes_from_dq_request& v);
    static ores::refdata::messaging::publish_asset_classes_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_asset_measures_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_asset_measures_from_dq_request& v);
    static ores::refdata::messaging::publish_asset_measures_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_asset_measures_from_dq_request& v);
    static ores::refdata::messaging::publish_asset_measures_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_benchmark_rates_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_benchmark_rates_from_dq_request& v);
    static ores::refdata::messaging::publish_benchmark_rates_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_benchmark_rates_from_dq_request& v);
    static ores::refdata::messaging::publish_benchmark_rates_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_books_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::publish_books_from_dq_request& v);
    static ores::refdata::messaging::publish_books_from_dq_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_books_from_dq_request& v);
    static ores::refdata::messaging::publish_books_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_business_centres_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_business_centres_from_dq_request& v);
    static ores::refdata::messaging::publish_business_centres_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_business_centres_from_dq_request& v);
    static ores::refdata::messaging::publish_business_centres_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_business_processes_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_business_processes_from_dq_request& v);
    static ores::refdata::messaging::publish_business_processes_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_business_processes_from_dq_request& v);
    static ores::refdata::messaging::publish_business_processes_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_business_units_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_business_units_from_dq_request& v);
    static ores::refdata::messaging::publish_business_units_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_business_units_from_dq_request& v);
    static ores::refdata::messaging::publish_business_units_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_calendar_events_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_calendar_events_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_events_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_calendar_events_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_events_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_calendar_exceptions_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_calendar_exceptions_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_exceptions_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_calendar_exceptions_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_exceptions_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_calendar_types_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_calendar_types_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_types_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_calendar_types_from_dq_request& v);
    static ores::refdata::messaging::publish_calendar_types_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_calendars_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_calendars_from_dq_request& v);
    static ores::refdata::messaging::publish_calendars_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_calendars_from_dq_request& v);
    static ores::refdata::messaging::publish_calendars_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_cashflow_types_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_cashflow_types_from_dq_request& v);
    static ores::refdata::messaging::publish_cashflow_types_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_cashflow_types_from_dq_request& v);
    static ores::refdata::messaging::publish_cashflow_types_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_commodities_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_commodities_from_dq_request& v);
    static ores::refdata::messaging::publish_commodities_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_commodities_from_dq_request& v);
    static ores::refdata::messaging::publish_commodities_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_legal_entities_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_legal_entities_from_dq_request& v);
    static ores::refdata::messaging::publish_legal_entities_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_legal_entities_from_dq_request& v);
    static ores::refdata::messaging::publish_legal_entities_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_equity_underlyings_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_equity_underlyings_from_dq_request& v);
    static ores::refdata::messaging::publish_equity_underlyings_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_equity_underlyings_from_dq_request& v);
    static ores::refdata::messaging::publish_equity_underlyings_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_conventions_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_conventions_from_dq_request& v);
    static ores::refdata::messaging::publish_conventions_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_conventions_from_dq_request& v);
    static ores::refdata::messaging::publish_conventions_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_counterparty_aliases_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_counterparty_aliases_from_dq_request& v);
    static ores::refdata::messaging::publish_counterparty_aliases_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_counterparty_aliases_from_dq_request& v);
    static ores::refdata::messaging::publish_counterparty_aliases_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_countries_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_countries_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_countries_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request& v);
    static ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request& v);
    static ores::refdata::messaging::publish_crm_topology_bundles_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::publish_csas_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::publish_csas_from_dq_request& v);
    static ores::refdata::messaging::publish_csas_from_dq_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_csas_from_dq_request& v);
    static ores::refdata::messaging::publish_csas_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currencies_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currencies_from_dq_request& v);
    static ores::refdata::messaging::publish_currencies_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_currencies_from_dq_request& v);
    static ores::refdata::messaging::publish_currencies_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_curve_configuration_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_curve_configuration_from_dq_request& v);
    static ores::refdata::messaging::publish_curve_configuration_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_curve_configuration_from_dq_request& v);
    static ores::refdata::messaging::publish_curve_configuration_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_groups_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currency_groups_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_groups_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_currency_groups_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_groups_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_currency_groups_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currency_currency_groups_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_currency_groups_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_currency_currency_groups_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_currency_groups_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_pairs_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currency_pairs_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_pairs_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_currency_pairs_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_pairs_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_pair_conventions_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_calendars_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currency_calendars_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_calendars_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_currency_calendars_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_calendars_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_currency_countries_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_currency_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_countries_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_currency_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_currency_countries_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request&
            v);
    static ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request&
            v);
    static ores::refdata::messaging::publish_currency_pair_convention_calendars_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_entity_classifications_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_entity_classifications_from_dq_request& v);
    static ores::refdata::messaging::publish_entity_classifications_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_entity_classifications_from_dq_request& v);
    static ores::refdata::messaging::publish_entity_classifications_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_lei_counterparties_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_lei_counterparties_from_dq_request& v);
    static ores::refdata::messaging::publish_lei_counterparties_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_lei_counterparties_from_dq_request& v);
    static ores::refdata::messaging::publish_lei_counterparties_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_lei_parties_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_lei_parties_from_dq_request& v);
    static ores::refdata::messaging::publish_lei_parties_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_lei_parties_from_dq_request& v);
    static ores::refdata::messaging::publish_lei_parties_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_local_jurisdictions_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_local_jurisdictions_from_dq_request& v);
    static ores::refdata::messaging::publish_local_jurisdictions_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_local_jurisdictions_from_dq_request& v);
    static ores::refdata::messaging::publish_local_jurisdictions_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request& v);
    static ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request& v);
    static ores::refdata::messaging::publish_sandbox_portfolios_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_netting_agreements_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_netting_agreements_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_agreements_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_netting_agreements_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_agreements_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_netting_set_aliases_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_netting_set_aliases_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_set_aliases_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_netting_set_aliases_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_set_aliases_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_netting_sets_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_netting_sets_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_sets_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_netting_sets_from_dq_request& v);
    static ores::refdata::messaging::publish_netting_sets_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_contact_informations_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::publish_party_contact_informations_from_dq_request& v);
    static ores::refdata::messaging::publish_party_contact_informations_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_party_contact_informations_from_dq_request& v);
    static ores::refdata::messaging::publish_party_contact_informations_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_counterparties_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_party_counterparties_from_dq_request& v);
    static ores::refdata::messaging::publish_party_counterparties_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_party_counterparties_from_dq_request& v);
    static ores::refdata::messaging::publish_party_counterparties_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_countries_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_party_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_party_countries_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_party_countries_from_dq_request& v);
    static ores::refdata::messaging::publish_party_countries_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_currencies_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_party_currencies_from_dq_request& v);
    static ores::refdata::messaging::publish_party_currencies_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_party_currencies_from_dq_request& v);
    static ores::refdata::messaging::publish_party_currencies_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_relationships_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_party_relationships_from_dq_request& v);
    static ores::refdata::messaging::publish_party_relationships_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_party_relationships_from_dq_request& v);
    static ores::refdata::messaging::publish_party_relationships_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_party_roles_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_party_roles_from_dq_request& v);
    static ores::refdata::messaging::publish_party_roles_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_party_roles_from_dq_request& v);
    static ores::refdata::messaging::publish_party_roles_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_payment_frequencies_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_payment_frequencies_from_dq_request& v);
    static ores::refdata::messaging::publish_payment_frequencies_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_payment_frequencies_from_dq_request& v);
    static ores::refdata::messaging::publish_payment_frequencies_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_person_roles_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_person_roles_from_dq_request& v);
    static ores::refdata::messaging::publish_person_roles_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_person_roles_from_dq_request& v);
    static ores::refdata::messaging::publish_person_roles_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_portfolios_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_portfolios_from_dq_request& v);
    static ores::refdata::messaging::publish_portfolios_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_portfolios_from_dq_request& v);
    static ores::refdata::messaging::publish_portfolios_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request& v);
    static ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request& v);
    static ores::refdata::messaging::publish_regulatory_corporate_sectors_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_reporting_regimes_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_reporting_regimes_from_dq_request& v);
    static ores::refdata::messaging::publish_reporting_regimes_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_reporting_regimes_from_dq_request& v);
    static ores::refdata::messaging::publish_reporting_regimes_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_supervisory_bodies_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_supervisory_bodies_from_dq_request& v);
    static ores::refdata::messaging::publish_supervisory_bodies_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_supervisory_bodies_from_dq_request& v);
    static ores::refdata::messaging::publish_supervisory_bodies_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::publish_tenor_schedules_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::publish_tenor_schedules_from_dq_request& v);
    static ores::refdata::messaging::publish_tenor_schedules_from_dq_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::publish_tenor_schedules_from_dq_request& v);
    static ores::refdata::messaging::publish_tenor_schedules_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
