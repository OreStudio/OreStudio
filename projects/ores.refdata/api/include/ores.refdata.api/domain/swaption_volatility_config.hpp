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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_SWAPTION_VOLATILITY_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_SWAPTION_VOLATILITY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE swaption volatility entry.
 *
 * The settings of one SwaptionVolatility entry of a curveconfig.xml, one row
 * per curve_definition in the SwaptionVolatilities section: the surface's
 * dimension and volatility type, the option and swap tenors of its ATM matrix and
 * smile, and the swap indices it is built on. Its Report is a row of
 * curve_report_configuration and its ParametricSmileConfiguration a row of
 * curve_parametric_smile.
 *
 * A ProxyConfig builds the surface from another one; it is one per entry, so
 * its fields are the proxy_ columns, and has_proxy_config says whether the
 * entry wrote it.
 */
struct swaption_volatility_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief Whether the surface is ATM only or has a Smile.
     */
    std::optional<std::string> dimension;

    /**
     * @brief Whether the input volatility is normal, lognormal or shifted lognormal.
     */
    std::optional<std::string> volatility_type;

    /**
     * @brief How the smile interpolates.
     */
    std::optional<std::string> interpolation;

    /**
     * @brief How the surface extrapolates.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief The volatility type the surface is converted to.
     */
    std::optional<std::string> output_volatility_type;

    /**
     * @brief The shift of the model, as the document writes it.
     */
    std::optional<std::string> model_shift;

    /**
     * @brief The shift of the output volatility, as the document writes it.
     */
    std::optional<std::string> output_shift;

    /**
     * @brief The day counter of the surface, as the document spells it.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief The surface's calendar, as the document spells it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief The business day convention of the surface's dates.
     */
    std::optional<std::string> business_day_convention;

    /**
     * @brief The option tenors of the ATM matrix, as ORE's comma separated text.
     */
    std::optional<std::string> option_tenors;

    /**
     * @brief The swap tenors of the ATM matrix, as ORE's comma separated text.
     */
    std::optional<std::string> swap_tenors;

    /**
     * @brief The swap index of short swap tenors.
     */
    std::optional<std::string> short_swap_index_base;

    /**
     * @brief The swap index of the other swap tenors.
     */
    std::optional<std::string> swap_index_base;

    /**
     * @brief The option tenors of the smile, as ORE's comma separated text.
     */
    std::optional<std::string> smile_option_tenors;

    /**
     * @brief The swap tenors of the smile, as ORE's comma separated text.
     */
    std::optional<std::string> smile_swap_tenors;

    /**
     * @brief The strike spreads of the smile, as ORE's comma separated text.
     */
    std::optional<std::string> smile_spreads;

    /**
     * @brief The tag the surface's quotes are named with.
     */
    std::optional<std::string> quote_tag;

    /**
     * @brief Whether the entry writes a ProxyConfig element.
     */
    bool has_proxy_config = false;

    /**
     * @brief The swaption volatility a proxy surface is built from.
     */
    std::optional<std::string> proxy_source_curve_id;

    /**
     * @brief The short swap index of the proxy's source.
     */
    std::optional<std::string> proxy_source_short_swap_index_base;

    /**
     * @brief The swap index of the proxy's source.
     */
    std::optional<std::string> proxy_source_swap_index_base;

    /**
     * @brief The short swap index of the proxy's target.
     */
    std::optional<std::string> proxy_target_short_swap_index_base;

    /**
     * @brief The swap index of the proxy's target.
     */
    std::optional<std::string> proxy_target_swap_index_base;

    /**
     * @brief Username of the person who last modified this swaption volatility config.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const swaption_volatility_config&,
                           const swaption_volatility_config&) = default;
};

/**
 * @brief Dispatch-key identifier for swaption_volatility_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const swaption_volatility_config&) {
    return "ores.refdata.swaption_volatility_config";
}

}

#endif
