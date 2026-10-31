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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_PARAMETRIC_SMILE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_PARAMETRIC_SMILE_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_parametric_smile.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct curve_parametric_smile_key {
    boost::uuids::uuid id;
};

struct curve_parametric_smile_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    int max_calibration_attempts;
    double exit_early_error_threshold;
    double max_acceptable_error;
    std::optional<std::string> residual_correction_dimension;
};

struct curve_parametric_smile_change {
    curve_parametric_smile_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_parametric_smile_removal {
    curve_parametric_smile_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_parametric_smile_lookup {
    curve_parametric_smile_key key;
    std::optional<ores::refdata::domain::curve_parametric_smile> curve_parametric_smile;
};

struct curve_parametric_smiles_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_parametric_smile_event {
    boost::uuids::uuid event_id;
    curve_parametric_smile_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_parametric_smile_version_key {
    curve_parametric_smile_key curve_parametric_smile;
    std::uint32_t version;
};

struct curve_parametric_smile_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_parametric_smiles_request {
    using response_type = struct list_curve_parametric_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.list";
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
    std::optional<curve_parametric_smiles_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_parametric_smiles_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile> parametric_smiles;
    std::uint64_t total;
};

struct get_curve_parametric_smile_request {
    using response_type = struct get_curve_parametric_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_key key;
};

struct get_curve_parametric_smile_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile> curve_parametric_smile;
};

struct get_many_curve_parametric_smiles_request {
    using response_type = struct get_many_curve_parametric_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_key> keys;
};

struct get_many_curve_parametric_smiles_response {
    ores::utility::domain::result result;
    std::vector<curve_parametric_smile_lookup> entries;
};

struct put_curve_parametric_smile_request {
    using response_type = struct put_curve_parametric_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_parametric_smile_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile> curve_parametric_smile;
};

struct put_many_curve_parametric_smiles_request {
    using response_type = struct put_many_curve_parametric_smiles_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_parametric_smiles_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile> parametric_smiles;
};

struct delete_curve_parametric_smile_request {
    using response_type = struct delete_curve_parametric_smile_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_parametric_smiles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_parametric_smile_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_parametric_smiles_request {
    using response_type = struct delete_many_curve_parametric_smiles_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smiles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_parametric_smiles_response {
    ores::utility::domain::result result;
};

struct list_curve_parametric_smile_versions_request {
    using response_type = struct list_curve_parametric_smile_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smiles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_parametric_smile_versions_filter> filter;
};

struct list_curve_parametric_smile_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile> versions;
    std::uint64_t total;
};

struct get_curve_parametric_smile_version_request {
    using response_type = struct get_curve_parametric_smile_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smiles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_version_key key;
};

struct get_curve_parametric_smile_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_parametric_smile_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_parametric_smiles_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_parametric_smiles_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_parametric_smiles_events.deleted";
}
/**
 * @brief The members of curve_parametric_smile_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_parametric_smile_key::id},
    };
}

/**
 * @brief The members of curve_parametric_smile_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_parametric_smile_write::id},
        utility::codec::field{"curve_definition_id",
                              &curve_parametric_smile_write::curve_definition_id},
        utility::codec::field{"max_calibration_attempts",
                              &curve_parametric_smile_write::max_calibration_attempts},
        utility::codec::field{"exit_early_error_threshold",
                              &curve_parametric_smile_write::exit_early_error_threshold},
        utility::codec::field{"max_acceptable_error",
                              &curve_parametric_smile_write::max_acceptable_error},
        utility::codec::field{"residual_correction_dimension",
                              &curve_parametric_smile_write::residual_correction_dimension},
    };
}

/**
 * @brief The members of curve_parametric_smile_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_parametric_smile_change::write},
        utility::codec::field{"precondition", &curve_parametric_smile_change::precondition},
    };
}

/**
 * @brief The members of curve_parametric_smile_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_parametric_smile_removal::key},
        utility::codec::field{"precondition", &curve_parametric_smile_removal::precondition},
    };
}

/**
 * @brief The members of curve_parametric_smile_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_parametric_smile_lookup::key},
        utility::codec::field{"curve_parametric_smile",
                              &curve_parametric_smile_lookup::curve_parametric_smile},
    };
}

/**
 * @brief The members of curve_parametric_smiles_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smiles_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_parametric_smiles_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_parametric_smile_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_parametric_smile_event::event_id},
        utility::codec::field{"key", &curve_parametric_smile_event::key},
        utility::codec::field{"action", &curve_parametric_smile_event::action},
        utility::codec::field{"version", &curve_parametric_smile_event::version},
        utility::codec::field{"occurred_at", &curve_parametric_smile_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_parametric_smile_event::correlation_id},
    };
}

/**
 * @brief The members of curve_parametric_smile_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_parametric_smile",
                              &curve_parametric_smile_version_key::curve_parametric_smile},
        utility::codec::field{"version", &curve_parametric_smile_version_key::version},
    };
}

/**
 * @brief The members of curve_parametric_smile_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_parametric_smile_versions_filter::version},
        utility::codec::field{"from_version",
                              &curve_parametric_smile_versions_filter::from_version},
        utility::codec::field{"to_version", &curve_parametric_smile_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_parametric_smiles_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smiles_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_parametric_smiles_request::offset},
        utility::codec::field{"limit", &list_curve_parametric_smiles_request::limit},
        utility::codec::field{"order", &list_curve_parametric_smiles_request::order},
        utility::codec::field{"filter", &list_curve_parametric_smiles_request::filter},
        utility::codec::field{"as_of", &list_curve_parametric_smiles_request::as_of},
    };
}

/**
 * @brief The members of list_curve_parametric_smiles_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smiles_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_parametric_smiles_response::result},
        utility::codec::field{"parametric_smiles",
                              &list_curve_parametric_smiles_response::parametric_smiles},
        utility::codec::field{"total", &list_curve_parametric_smiles_response::total},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_parametric_smile_request::key},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_parametric_smile_response::result},
        utility::codec::field{"curve_parametric_smile",
                              &get_curve_parametric_smile_response::curve_parametric_smile},
    };
}

/**
 * @brief The members of get_many_curve_parametric_smiles_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_parametric_smiles_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_parametric_smiles_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_parametric_smiles_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_parametric_smiles_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_parametric_smiles_response::result},
        utility::codec::field{"entries", &get_many_curve_parametric_smiles_response::entries},
    };
}

/**
 * @brief The members of put_curve_parametric_smile_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_parametric_smile_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_parametric_smile_request::change},
        utility::codec::field{"intent", &put_curve_parametric_smile_request::intent},
    };
}

/**
 * @brief The members of put_curve_parametric_smile_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_parametric_smile_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_parametric_smile_response::result},
        utility::codec::field{"curve_parametric_smile",
                              &put_curve_parametric_smile_response::curve_parametric_smile},
    };
}

/**
 * @brief The members of put_many_curve_parametric_smiles_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_parametric_smiles_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_parametric_smiles_request::changes},
        utility::codec::field{"intent", &put_many_curve_parametric_smiles_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_parametric_smiles_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_parametric_smiles_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_parametric_smiles_response::result},
        utility::codec::field{"parametric_smiles",
                              &put_many_curve_parametric_smiles_response::parametric_smiles},
    };
}

/**
 * @brief The members of delete_curve_parametric_smile_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_parametric_smile_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_parametric_smile_request::removal},
        utility::codec::field{"intent", &delete_curve_parametric_smile_request::intent},
    };
}

/**
 * @brief The members of delete_curve_parametric_smile_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_parametric_smile_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_parametric_smile_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_parametric_smiles_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_parametric_smiles_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_curve_parametric_smiles_request::removals},
        utility::codec::field{"intent", &delete_many_curve_parametric_smiles_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_parametric_smiles_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_parametric_smiles_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_curve_parametric_smiles_response::result},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smile_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_parametric_smile_versions_request::key},
        utility::codec::field{"offset", &list_curve_parametric_smile_versions_request::offset},
        utility::codec::field{"limit", &list_curve_parametric_smile_versions_request::limit},
        utility::codec::field{"order", &list_curve_parametric_smile_versions_request::order},
        utility::codec::field{"filter", &list_curve_parametric_smile_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smile_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_parametric_smile_versions_response::result},
        utility::codec::field{"versions", &list_curve_parametric_smile_versions_response::versions},
        utility::codec::field{"total", &list_curve_parametric_smile_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_parametric_smile_version_request::key},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_parametric_smile_version_response::result},
        utility::codec::field{"version", &get_curve_parametric_smile_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_parametric_smile_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_parametric_smile_key& v);
    static ores::refdata::messaging::curve_parametric_smile_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_parametric_smile_key& v);
    static ores::refdata::messaging::curve_parametric_smile_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_parametric_smile_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_parametric_smile_write& v);
    static ores::refdata::messaging::curve_parametric_smile_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_write& v);
    static ores::refdata::messaging::curve_parametric_smile_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_parametric_smile_change& v);
    static ores::refdata::messaging::curve_parametric_smile_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_change& v);
    static ores::refdata::messaging::curve_parametric_smile_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_removal& v);
    static ores::refdata::messaging::curve_parametric_smile_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_removal& v);
    static ores::refdata::messaging::curve_parametric_smile_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_parametric_smile_lookup& v);
    static ores::refdata::messaging::curve_parametric_smile_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_lookup& v);
    static ores::refdata::messaging::curve_parametric_smile_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smiles_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smiles_filter& v);
    static ores::refdata::messaging::curve_parametric_smiles_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smiles_filter& v);
    static ores::refdata::messaging::curve_parametric_smiles_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_parametric_smile_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_parametric_smile_event& v);
    static ores::refdata::messaging::curve_parametric_smile_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_event& v);
    static ores::refdata::messaging::curve_parametric_smile_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_version_key& v);
    static ores::refdata::messaging::curve_parametric_smile_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_version_key& v);
    static ores::refdata::messaging::curve_parametric_smile_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_versions_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_versions_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smiles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::list_curve_parametric_smiles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::list_curve_parametric_smiles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smiles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::list_curve_parametric_smiles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::list_curve_parametric_smiles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_parametric_smiles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::get_many_curve_parametric_smiles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::get_many_curve_parametric_smiles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_parametric_smiles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::get_many_curve_parametric_smiles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::get_many_curve_parametric_smiles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_parametric_smile_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_parametric_smile_request& v);
    static ores::refdata::messaging::put_curve_parametric_smile_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_parametric_smile_request& v);
    static ores::refdata::messaging::put_curve_parametric_smile_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_parametric_smile_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_parametric_smile_response& v);
    static ores::refdata::messaging::put_curve_parametric_smile_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_parametric_smile_response& v);
    static ores::refdata::messaging::put_curve_parametric_smile_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_parametric_smiles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::put_many_curve_parametric_smiles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::put_many_curve_parametric_smiles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_parametric_smiles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::put_many_curve_parametric_smiles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::put_many_curve_parametric_smiles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_parametric_smile_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_parametric_smile_request& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_parametric_smile_request& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_parametric_smile_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_parametric_smile_response& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_parametric_smile_response& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_parametric_smiles_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smiles_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_parametric_smiles_request& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smiles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_parametric_smiles_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smiles_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_parametric_smiles_response& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smiles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smile_versions_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_parametric_smile_versions_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smile_versions_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_parametric_smile_versions_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_version_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_version_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_version_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_version_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
