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
#ifndef ORES_REFDATA_API_MESSAGING_FX_VOLATILITY_SMILE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_FX_VOLATILITY_SMILE_PROTOCOL_HPP

#include "ores.refdata.api/domain/fx_volatility_smile.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct fx_volatility_smile_key {
    boost::uuids::uuid id;
};

struct fx_volatility_smile_write {
    boost::uuids::uuid id;
    boost::uuids::uuid fx_volatility_config_id;
    std::optional<std::string> smile_type;
    std::optional<std::string> smile_interpolation;
    std::optional<std::string> smile_delta;
    std::optional<std::string> smile_extrapolation;
    std::optional<std::string> time_interpolation;
    std::optional<std::string> time_weighting;
    std::optional<double> butterfly_error_tolerance;
};

struct fx_volatility_smile_change {
    fx_volatility_smile_write write;
    ores::utility::domain::precondition precondition;
};

struct fx_volatility_smile_removal {
    fx_volatility_smile_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fx_volatility_smile_lookup {
    fx_volatility_smile_key key;
    std::optional<ores::refdata::domain::fx_volatility_smile> fx_volatility_smile;
};

struct fx_volatility_smiles_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct fx_volatility_smile_event {
    boost::uuids::uuid event_id;
    fx_volatility_smile_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fx_volatility_smile_version_key {
    fx_volatility_smile_key fx_volatility_smile;
    std::uint32_t version;
};

struct fx_volatility_smile_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fx_volatility_smiles_request {
    using response_type = struct list_fx_volatility_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_volatility_smiles_filter> filter;
    std::optional<std::string> as_of;
};

struct list_fx_volatility_smiles_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_volatility_smile> fx_volatility_smiles;
    std::uint64_t total;
};

struct get_fx_volatility_smile_request {
    using response_type = struct get_fx_volatility_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_volatility_smile_key key;
};

struct get_fx_volatility_smile_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_volatility_smile> fx_volatility_smile;
};

struct get_many_fx_volatility_smiles_request {
    using response_type = struct get_many_fx_volatility_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_volatility_smile_key> keys;
};

struct get_many_fx_volatility_smiles_response {
    ores::utility::domain::result result;
    std::vector<fx_volatility_smile_lookup> entries;
};

struct put_fx_volatility_smile_request {
    using response_type = struct put_fx_volatility_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_volatility_smile_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fx_volatility_smile_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_volatility_smile> fx_volatility_smile;
};

struct put_many_fx_volatility_smiles_request {
    using response_type = struct put_many_fx_volatility_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_volatility_smile_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fx_volatility_smiles_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_volatility_smile> fx_volatility_smiles;
};

struct delete_fx_volatility_smile_request {
    using response_type = struct delete_fx_volatility_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_volatility_smile_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fx_volatility_smile_response {
    ores::utility::domain::result result;
};

struct delete_many_fx_volatility_smiles_request {
    using response_type = struct delete_many_fx_volatility_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_volatility_smile_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fx_volatility_smiles_response {
    ores::utility::domain::result result;
};

struct list_fx_volatility_smile_versions_request {
    using response_type = struct list_fx_volatility_smile_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.fx_volatility_smiles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_volatility_smile_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_volatility_smile_versions_filter> filter;
};

struct list_fx_volatility_smile_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::fx_volatility_smile> versions;
    std::uint64_t total;
};

struct get_fx_volatility_smile_version_request {
    using response_type = struct get_fx_volatility_smile_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.fx_volatility_smiles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_volatility_smile_version_key key;
};

struct get_fx_volatility_smile_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::fx_volatility_smile> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_volatility_smile_event_subjects {
inline constexpr std::string_view created = "refdata.v1.fx_volatility_smiles_events.created";
inline constexpr std::string_view updated = "refdata.v1.fx_volatility_smiles_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.fx_volatility_smiles_events.deleted";
}

}

#endif
