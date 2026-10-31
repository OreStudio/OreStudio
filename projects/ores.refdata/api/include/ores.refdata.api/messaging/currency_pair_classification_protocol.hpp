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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CLASSIFICATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CLASSIFICATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair_classification.hpp"
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

struct currency_pair_classification_key {
    std::string code;
};

struct currency_pair_classification_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct currency_pair_classification_change {
    currency_pair_classification_write write;
    ores::utility::domain::precondition precondition;
};

struct currency_pair_classification_removal {
    currency_pair_classification_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct currency_pair_classification_lookup {
    currency_pair_classification_key key;
    std::optional<ores::refdata::domain::currency_pair_classification> currency_pair_classification;
};

struct currency_pair_classifications_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct currency_pair_classification_event {
    boost::uuids::uuid event_id;
    currency_pair_classification_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct currency_pair_classification_version_key {
    currency_pair_classification_key currency_pair_classification;
    std::uint32_t version;
};

struct currency_pair_classification_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_currency_pair_classifications_request {
    using response_type = struct list_currency_pair_classifications_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.list";
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
    std::optional<currency_pair_classifications_filter> filter;
    std::optional<std::string> as_of;
};

struct list_currency_pair_classifications_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_classification> classifications;
    std::uint64_t total;
};

struct get_currency_pair_classification_request {
    using response_type = struct get_currency_pair_classification_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_classifications.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_classification_key key;
};

struct get_currency_pair_classification_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair_classification> currency_pair_classification;
};

struct get_many_currency_pair_classifications_request {
    using response_type = struct get_many_currency_pair_classifications_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_classification_key> keys;
};

struct get_many_currency_pair_classifications_response {
    ores::utility::domain::result result;
    std::vector<currency_pair_classification_lookup> entries;
};

struct put_currency_pair_classification_request {
    using response_type = struct put_currency_pair_classification_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pair_classifications.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_classification_change change;
    ores::utility::domain::change_intent intent;
};

struct put_currency_pair_classification_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair_classification> currency_pair_classification;
};

struct put_many_currency_pair_classifications_request {
    using response_type = struct put_many_currency_pair_classifications_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_classification_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_currency_pair_classifications_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_classification> classifications;
};

struct delete_currency_pair_classification_request {
    using response_type = struct delete_currency_pair_classification_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_classification_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_currency_pair_classification_response {
    ores::utility::domain::result result;
};

struct delete_many_currency_pair_classifications_request {
    using response_type = struct delete_many_currency_pair_classifications_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_classification_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_currency_pair_classifications_response {
    ores::utility::domain::result result;
};

struct list_currency_pair_classification_versions_request {
    using response_type = struct list_currency_pair_classification_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_classification_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<currency_pair_classification_versions_filter> filter;
};

struct list_currency_pair_classification_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_classification> versions;
    std::uint64_t total;
};

struct get_currency_pair_classification_version_request {
    using response_type = struct get_currency_pair_classification_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_classifications_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_classification_version_key key;
};

struct get_currency_pair_classification_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair_classification> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace currency_pair_classification_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.currency_pair_classifications_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.currency_pair_classifications_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.currency_pair_classifications_events.deleted";
}
/**
 * @brief The members of currency_pair_classification_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_key&) {
    return std::tuple{
        utility::codec::field{"code", &currency_pair_classification_key::code},
    };
}

/**
 * @brief The members of currency_pair_classification_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_write&) {
    return std::tuple{
        utility::codec::field{"code", &currency_pair_classification_write::code},
        utility::codec::field{"name", &currency_pair_classification_write::name},
        utility::codec::field{"description", &currency_pair_classification_write::description},
        utility::codec::field{"display_order", &currency_pair_classification_write::display_order},
    };
}

/**
 * @brief The members of currency_pair_classification_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_change&) {
    return std::tuple{
        utility::codec::field{"write", &currency_pair_classification_change::write},
        utility::codec::field{"precondition", &currency_pair_classification_change::precondition},
    };
}

/**
 * @brief The members of currency_pair_classification_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_removal&) {
    return std::tuple{
        utility::codec::field{"key", &currency_pair_classification_removal::key},
        utility::codec::field{"precondition", &currency_pair_classification_removal::precondition},
    };
}

/**
 * @brief The members of currency_pair_classification_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &currency_pair_classification_lookup::key},
        utility::codec::field{"currency_pair_classification",
                              &currency_pair_classification_lookup::currency_pair_classification},
    };
}

/**
 * @brief The members of currency_pair_classifications_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classifications_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &currency_pair_classifications_filter::code_one_of},
    };
}

/**
 * @brief The members of currency_pair_classification_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &currency_pair_classification_event::event_id},
        utility::codec::field{"key", &currency_pair_classification_event::key},
        utility::codec::field{"action", &currency_pair_classification_event::action},
        utility::codec::field{"version", &currency_pair_classification_event::version},
        utility::codec::field{"occurred_at", &currency_pair_classification_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &currency_pair_classification_event::correlation_id},
    };
}

/**
 * @brief The members of currency_pair_classification_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_version_key&) {
    return std::tuple{
        utility::codec::field{
            "currency_pair_classification",
            &currency_pair_classification_version_key::currency_pair_classification},
        utility::codec::field{"version", &currency_pair_classification_version_key::version},
    };
}

/**
 * @brief The members of currency_pair_classification_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_classification_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &currency_pair_classification_versions_filter::version},
        utility::codec::field{"from_version",
                              &currency_pair_classification_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &currency_pair_classification_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_currency_pair_classifications_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_pair_classifications_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_currency_pair_classifications_request::offset},
        utility::codec::field{"limit", &list_currency_pair_classifications_request::limit},
        utility::codec::field{"order", &list_currency_pair_classifications_request::order},
        utility::codec::field{"filter", &list_currency_pair_classifications_request::filter},
        utility::codec::field{"as_of", &list_currency_pair_classifications_request::as_of},
    };
}

/**
 * @brief The members of list_currency_pair_classifications_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_pair_classifications_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_currency_pair_classifications_response::result},
        utility::codec::field{"classifications",
                              &list_currency_pair_classifications_response::classifications},
        utility::codec::field{"total", &list_currency_pair_classifications_response::total},
    };
}

/**
 * @brief The members of get_currency_pair_classification_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_classification_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_currency_pair_classification_request::key},
    };
}

/**
 * @brief The members of get_currency_pair_classification_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_classification_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_currency_pair_classification_response::result},
        utility::codec::field{
            "currency_pair_classification",
            &get_currency_pair_classification_response::currency_pair_classification},
    };
}

/**
 * @brief The members of get_many_currency_pair_classifications_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_currency_pair_classifications_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_currency_pair_classifications_request::keys},
    };
}

/**
 * @brief The members of get_many_currency_pair_classifications_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_currency_pair_classifications_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_currency_pair_classifications_response::result},
        utility::codec::field{"entries", &get_many_currency_pair_classifications_response::entries},
    };
}

/**
 * @brief The members of put_currency_pair_classification_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_pair_classification_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_currency_pair_classification_request::change},
        utility::codec::field{"intent", &put_currency_pair_classification_request::intent},
    };
}

/**
 * @brief The members of put_currency_pair_classification_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_pair_classification_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_currency_pair_classification_response::result},
        utility::codec::field{
            "currency_pair_classification",
            &put_currency_pair_classification_response::currency_pair_classification},
    };
}

/**
 * @brief The members of put_many_currency_pair_classifications_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_currency_pair_classifications_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_currency_pair_classifications_request::changes},
        utility::codec::field{"intent", &put_many_currency_pair_classifications_request::intent},
    };
}

/**
 * @brief The members of put_many_currency_pair_classifications_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_currency_pair_classifications_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_currency_pair_classifications_response::result},
        utility::codec::field{"classifications",
                              &put_many_currency_pair_classifications_response::classifications},
    };
}

/**
 * @brief The members of delete_currency_pair_classification_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_pair_classification_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_currency_pair_classification_request::removal},
        utility::codec::field{"intent", &delete_currency_pair_classification_request::intent},
    };
}

/**
 * @brief The members of delete_currency_pair_classification_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_pair_classification_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_currency_pair_classification_response::result},
    };
}

/**
 * @brief The members of delete_many_currency_pair_classifications_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_currency_pair_classifications_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_currency_pair_classifications_request::removals},
        utility::codec::field{"intent", &delete_many_currency_pair_classifications_request::intent},
    };
}

/**
 * @brief The members of delete_many_currency_pair_classifications_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_currency_pair_classifications_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_currency_pair_classifications_response::result},
    };
}

/**
 * @brief The members of list_currency_pair_classification_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_currency_pair_classification_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_currency_pair_classification_versions_request::key},
        utility::codec::field{"offset",
                              &list_currency_pair_classification_versions_request::offset},
        utility::codec::field{"limit", &list_currency_pair_classification_versions_request::limit},
        utility::codec::field{"order", &list_currency_pair_classification_versions_request::order},
        utility::codec::field{"filter",
                              &list_currency_pair_classification_versions_request::filter},
    };
}

/**
 * @brief The members of list_currency_pair_classification_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_currency_pair_classification_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_currency_pair_classification_versions_response::result},
        utility::codec::field{"versions",
                              &list_currency_pair_classification_versions_response::versions},
        utility::codec::field{"total", &list_currency_pair_classification_versions_response::total},
    };
}

/**
 * @brief The members of get_currency_pair_classification_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_classification_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_currency_pair_classification_version_request::key},
    };
}

/**
 * @brief The members of get_currency_pair_classification_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_classification_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_currency_pair_classification_version_response::result},
        utility::codec::field{"version",
                              &get_currency_pair_classification_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_key& v);
    static ores::refdata::messaging::currency_pair_classification_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_key& v);
    static ores::refdata::messaging::currency_pair_classification_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_write& v);
    static ores::refdata::messaging::currency_pair_classification_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_write& v);
    static ores::refdata::messaging::currency_pair_classification_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_change& v);
    static ores::refdata::messaging::currency_pair_classification_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_change& v);
    static ores::refdata::messaging::currency_pair_classification_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_removal& v);
    static ores::refdata::messaging::currency_pair_classification_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_removal& v);
    static ores::refdata::messaging::currency_pair_classification_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_lookup& v);
    static ores::refdata::messaging::currency_pair_classification_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_lookup& v);
    static ores::refdata::messaging::currency_pair_classification_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classifications_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classifications_filter& v);
    static ores::refdata::messaging::currency_pair_classifications_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classifications_filter& v);
    static ores::refdata::messaging::currency_pair_classifications_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_event& v);
    static ores::refdata::messaging::currency_pair_classification_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_event& v);
    static ores::refdata::messaging::currency_pair_classification_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_version_key& v);
    static ores::refdata::messaging::currency_pair_classification_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_version_key& v);
    static ores::refdata::messaging::currency_pair_classification_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_classification_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_classification_versions_filter& v);
    static ores::refdata::messaging::currency_pair_classification_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_classification_versions_filter& v);
    static ores::refdata::messaging::currency_pair_classification_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_classifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_pair_classifications_request& v);
    static ores::refdata::messaging::list_currency_pair_classifications_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_currency_pair_classifications_request& v);
    static ores::refdata::messaging::list_currency_pair_classifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_classifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_pair_classifications_response& v);
    static ores::refdata::messaging::list_currency_pair_classifications_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_currency_pair_classifications_response& v);
    static ores::refdata::messaging::list_currency_pair_classifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_classification_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_currency_pair_classification_request& v);
    static ores::refdata::messaging::get_currency_pair_classification_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_currency_pair_classification_request& v);
    static ores::refdata::messaging::get_currency_pair_classification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_classification_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_currency_pair_classification_response& v);
    static ores::refdata::messaging::get_currency_pair_classification_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_currency_pair_classification_response& v);
    static ores::refdata::messaging::get_currency_pair_classification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_currency_pair_classifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::get_many_currency_pair_classifications_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::get_many_currency_pair_classifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_currency_pair_classifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::get_many_currency_pair_classifications_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::get_many_currency_pair_classifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_currency_pair_classification_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_currency_pair_classification_request& v);
    static ores::refdata::messaging::put_currency_pair_classification_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_currency_pair_classification_request& v);
    static ores::refdata::messaging::put_currency_pair_classification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_currency_pair_classification_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_currency_pair_classification_response& v);
    static ores::refdata::messaging::put_currency_pair_classification_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_currency_pair_classification_response& v);
    static ores::refdata::messaging::put_currency_pair_classification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_currency_pair_classifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::put_many_currency_pair_classifications_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::put_many_currency_pair_classifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_currency_pair_classifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::put_many_currency_pair_classifications_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::put_many_currency_pair_classifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_currency_pair_classification_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_currency_pair_classification_request& v);
    static ores::refdata::messaging::delete_currency_pair_classification_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_currency_pair_classification_request& v);
    static ores::refdata::messaging::delete_currency_pair_classification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_currency_pair_classification_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_currency_pair_classification_response& v);
    static ores::refdata::messaging::delete_currency_pair_classification_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_currency_pair_classification_response& v);
    static ores::refdata::messaging::delete_currency_pair_classification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currency_pair_classifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::delete_many_currency_pair_classifications_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_currency_pair_classifications_request& v);
    static ores::refdata::messaging::delete_many_currency_pair_classifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currency_pair_classifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::delete_many_currency_pair_classifications_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_currency_pair_classifications_response& v);
    static ores::refdata::messaging::delete_many_currency_pair_classifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_classification_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_currency_pair_classification_versions_request& v);
    static ores::refdata::messaging::list_currency_pair_classification_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_currency_pair_classification_versions_request& v);
    static ores::refdata::messaging::list_currency_pair_classification_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_classification_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_currency_pair_classification_versions_response& v);
    static ores::refdata::messaging::list_currency_pair_classification_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_currency_pair_classification_versions_response& v);
    static ores::refdata::messaging::list_currency_pair_classification_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_classification_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_currency_pair_classification_version_request& v);
    static ores::refdata::messaging::get_currency_pair_classification_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_currency_pair_classification_version_request& v);
    static ores::refdata::messaging::get_currency_pair_classification_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_classification_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_currency_pair_classification_version_response& v);
    static ores::refdata::messaging::get_currency_pair_classification_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_currency_pair_classification_version_response& v);
    static ores::refdata::messaging::get_currency_pair_classification_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
