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
#ifndef ORES_DQ_API_MESSAGING_DATASET_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_PROTOCOL_HPP

#include "ores.dq.api/domain/dataset.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct dataset_key {
    std::string code;
};

struct dataset_write {
    boost::uuids::uuid id;
    std::string code;
    std::optional<std::string> catalog_name;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> coding_scheme_code;
    std::string origin_code;
    std::string nature_code;
    std::string treatment_code;
    std::optional<boost::uuids::uuid> methodology_id;
    std::string name;
    std::string description;
    std::string source_system_id;
    std::string business_context;
    std::optional<boost::uuids::uuid> upstream_derivation_id;
    int lineage_depth;
    std::chrono::system_clock::time_point as_of_date;
    std::chrono::system_clock::time_point ingestion_timestamp;
    std::optional<std::string> license_info;
    std::string artefact_type;
};

struct dataset_change {
    dataset_write write;
    ores::utility::domain::precondition precondition;
};

struct dataset_removal {
    dataset_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct dataset_lookup {
    dataset_key key;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct datasets_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct dataset_event {
    boost::uuids::uuid event_id;
    dataset_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct dataset_version_key {
    dataset_key dataset;
    std::uint32_t version;
};

struct dataset_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_datasets_request {
    using response_type = struct list_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.list";
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
    std::optional<datasets_filter> filter;
    std::optional<std::string> as_of;
};

struct list_datasets_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> datasets;
    std::uint64_t total;
};

struct get_dataset_request {
    using response_type = struct get_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_key key;
};

struct get_dataset_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct get_many_datasets_request {
    using response_type = struct get_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_key> keys;
};

struct get_many_datasets_response {
    ores::utility::domain::result result;
    std::vector<dataset_lookup> entries;
};

struct put_dataset_request {
    using response_type = struct put_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_change change;
    ores::utility::domain::change_intent intent;
};

struct put_dataset_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct put_many_datasets_request {
    using response_type = struct put_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_datasets_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> datasets;
};

struct delete_dataset_request {
    using response_type = struct delete_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_dataset_response {
    ores::utility::domain::result result;
};

struct delete_many_datasets_request {
    using response_type = struct delete_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_datasets_response {
    ores::utility::domain::result result;
};

struct list_dataset_versions_request {
    using response_type = struct list_dataset_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<dataset_versions_filter> filter;
};

struct list_dataset_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> versions;
    std::uint64_t total;
};

struct get_dataset_version_request {
    using response_type = struct get_dataset_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_version_key key;
};

struct get_dataset_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace dataset_event_subjects {
inline constexpr std::string_view created = "dq.v1.datasets_events.created";
inline constexpr std::string_view updated = "dq.v1.datasets_events.updated";
inline constexpr std::string_view deleted = "dq.v1.datasets_events.deleted";
}
/**
 * @brief The members of dataset_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_key&) {
    return std::tuple{
        utility::codec::field{"code", &dataset_key::code},
    };
}

/**
 * @brief The members of dataset_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_write&) {
    return std::tuple{
        utility::codec::field{"id", &dataset_write::id},
        utility::codec::field{"code", &dataset_write::code},
        utility::codec::field{"catalog_name", &dataset_write::catalog_name},
        utility::codec::field{"subject_area_name", &dataset_write::subject_area_name},
        utility::codec::field{"domain_name", &dataset_write::domain_name},
        utility::codec::field{"coding_scheme_code", &dataset_write::coding_scheme_code},
        utility::codec::field{"origin_code", &dataset_write::origin_code},
        utility::codec::field{"nature_code", &dataset_write::nature_code},
        utility::codec::field{"treatment_code", &dataset_write::treatment_code},
        utility::codec::field{"methodology_id", &dataset_write::methodology_id},
        utility::codec::field{"name", &dataset_write::name},
        utility::codec::field{"description", &dataset_write::description},
        utility::codec::field{"source_system_id", &dataset_write::source_system_id},
        utility::codec::field{"business_context", &dataset_write::business_context},
        utility::codec::field{"upstream_derivation_id", &dataset_write::upstream_derivation_id},
        utility::codec::field{"lineage_depth", &dataset_write::lineage_depth},
        utility::codec::field{"as_of_date", &dataset_write::as_of_date},
        utility::codec::field{"ingestion_timestamp", &dataset_write::ingestion_timestamp},
        utility::codec::field{"license_info", &dataset_write::license_info},
        utility::codec::field{"artefact_type", &dataset_write::artefact_type},
    };
}

/**
 * @brief The members of dataset_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_change&) {
    return std::tuple{
        utility::codec::field{"write", &dataset_change::write},
        utility::codec::field{"precondition", &dataset_change::precondition},
    };
}

/**
 * @brief The members of dataset_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_removal&) {
    return std::tuple{
        utility::codec::field{"key", &dataset_removal::key},
        utility::codec::field{"precondition", &dataset_removal::precondition},
    };
}

/**
 * @brief The members of dataset_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &dataset_lookup::key},
        utility::codec::field{"dataset", &dataset_lookup::dataset},
    };
}

/**
 * @brief The members of datasets_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const datasets_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &datasets_filter::id_one_of},
    };
}

/**
 * @brief The members of dataset_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &dataset_event::event_id},
        utility::codec::field{"key", &dataset_event::key},
        utility::codec::field{"action", &dataset_event::action},
        utility::codec::field{"version", &dataset_event::version},
        utility::codec::field{"occurred_at", &dataset_event::occurred_at},
        utility::codec::field{"correlation_id", &dataset_event::correlation_id},
    };
}

/**
 * @brief The members of dataset_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_version_key&) {
    return std::tuple{
        utility::codec::field{"dataset", &dataset_version_key::dataset},
        utility::codec::field{"version", &dataset_version_key::version},
    };
}

/**
 * @brief The members of dataset_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &dataset_versions_filter::version},
        utility::codec::field{"from_version", &dataset_versions_filter::from_version},
        utility::codec::field{"to_version", &dataset_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_datasets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_datasets_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_datasets_request::offset},
        utility::codec::field{"limit", &list_datasets_request::limit},
        utility::codec::field{"order", &list_datasets_request::order},
        utility::codec::field{"filter", &list_datasets_request::filter},
        utility::codec::field{"as_of", &list_datasets_request::as_of},
    };
}

/**
 * @brief The members of list_datasets_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_datasets_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_datasets_response::result},
        utility::codec::field{"datasets", &list_datasets_response::datasets},
        utility::codec::field{"total", &list_datasets_response::total},
    };
}

/**
 * @brief The members of get_dataset_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_dataset_request::key},
    };
}

/**
 * @brief The members of get_dataset_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_dataset_response::result},
        utility::codec::field{"dataset", &get_dataset_response::dataset},
    };
}

/**
 * @brief The members of get_many_datasets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_datasets_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_datasets_request::keys},
    };
}

/**
 * @brief The members of get_many_datasets_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_datasets_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_datasets_response::result},
        utility::codec::field{"entries", &get_many_datasets_response::entries},
    };
}

/**
 * @brief The members of put_dataset_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_dataset_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_dataset_request::change},
        utility::codec::field{"intent", &put_dataset_request::intent},
    };
}

/**
 * @brief The members of put_dataset_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_dataset_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_dataset_response::result},
        utility::codec::field{"dataset", &put_dataset_response::dataset},
    };
}

/**
 * @brief The members of put_many_datasets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_datasets_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_datasets_request::changes},
        utility::codec::field{"intent", &put_many_datasets_request::intent},
    };
}

/**
 * @brief The members of put_many_datasets_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_datasets_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_datasets_response::result},
        utility::codec::field{"datasets", &put_many_datasets_response::datasets},
    };
}

/**
 * @brief The members of delete_dataset_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_dataset_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_dataset_request::removal},
        utility::codec::field{"intent", &delete_dataset_request::intent},
    };
}

/**
 * @brief The members of delete_dataset_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_dataset_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_dataset_response::result},
    };
}

/**
 * @brief The members of delete_many_datasets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_datasets_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_datasets_request::removals},
        utility::codec::field{"intent", &delete_many_datasets_request::intent},
    };
}

/**
 * @brief The members of delete_many_datasets_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_datasets_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_datasets_response::result},
    };
}

/**
 * @brief The members of list_dataset_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_dataset_versions_request::key},
        utility::codec::field{"offset", &list_dataset_versions_request::offset},
        utility::codec::field{"limit", &list_dataset_versions_request::limit},
        utility::codec::field{"order", &list_dataset_versions_request::order},
        utility::codec::field{"filter", &list_dataset_versions_request::filter},
    };
}

/**
 * @brief The members of list_dataset_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_dataset_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_dataset_versions_response::result},
        utility::codec::field{"versions", &list_dataset_versions_response::versions},
        utility::codec::field{"total", &list_dataset_versions_response::total},
    };
}

/**
 * @brief The members of get_dataset_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_dataset_version_request::key},
    };
}

/**
 * @brief The members of get_dataset_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_dataset_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_dataset_version_response::result},
        utility::codec::field{"version", &get_dataset_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_key& v);
    static ores::dq::messaging::dataset_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_key& v);
    static ores::dq::messaging::dataset_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_write& v);
    static ores::dq::messaging::dataset_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_write& v);
    static ores::dq::messaging::dataset_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_change& v);
    static ores::dq::messaging::dataset_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_change& v);
    static ores::dq::messaging::dataset_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_removal& v);
    static ores::dq::messaging::dataset_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_removal& v);
    static ores::dq::messaging::dataset_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_lookup& v);
    static ores::dq::messaging::dataset_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_lookup& v);
    static ores::dq::messaging::dataset_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::datasets_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::datasets_filter& v);
    static ores::dq::messaging::datasets_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::datasets_filter& v);
    static ores::dq::messaging::datasets_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_event& v);
    static ores::dq::messaging::dataset_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_event& v);
    static ores::dq::messaging::dataset_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_version_key& v);
    static ores::dq::messaging::dataset_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_version_key& v);
    static ores::dq::messaging::dataset_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_versions_filter& v);
    static ores::dq::messaging::dataset_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_versions_filter& v);
    static ores::dq::messaging::dataset_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_datasets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_datasets_request& v);
    static ores::dq::messaging::list_datasets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_datasets_request& v);
    static ores::dq::messaging::list_datasets_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_datasets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_datasets_response& v);
    static ores::dq::messaging::list_datasets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_datasets_response& v);
    static ores::dq::messaging::list_datasets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_request& v);
    static ores::dq::messaging::get_dataset_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_request& v);
    static ores::dq::messaging::get_dataset_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_response& v);
    static ores::dq::messaging::get_dataset_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_response& v);
    static ores::dq::messaging::get_dataset_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_datasets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_datasets_request& v);
    static ores::dq::messaging::get_many_datasets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_datasets_request& v);
    static ores::dq::messaging::get_many_datasets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_datasets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_datasets_response& v);
    static ores::dq::messaging::get_many_datasets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_datasets_response& v);
    static ores::dq::messaging::get_many_datasets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_request& v);
    static ores::dq::messaging::put_dataset_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_dataset_request& v);
    static ores::dq::messaging::put_dataset_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_dataset_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_dataset_response& v);
    static ores::dq::messaging::put_dataset_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_dataset_response& v);
    static ores::dq::messaging::put_dataset_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_datasets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_datasets_request& v);
    static ores::dq::messaging::put_many_datasets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_datasets_request& v);
    static ores::dq::messaging::put_many_datasets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_datasets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_datasets_response& v);
    static ores::dq::messaging::put_many_datasets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_datasets_response& v);
    static ores::dq::messaging::put_many_datasets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_dataset_request& v);
    static ores::dq::messaging::delete_dataset_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_dataset_request& v);
    static ores::dq::messaging::delete_dataset_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_dataset_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_dataset_response& v);
    static ores::dq::messaging::delete_dataset_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_dataset_response& v);
    static ores::dq::messaging::delete_dataset_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_datasets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_datasets_request& v);
    static ores::dq::messaging::delete_many_datasets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_datasets_request& v);
    static ores::dq::messaging::delete_many_datasets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_datasets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_datasets_response& v);
    static ores::dq::messaging::delete_many_datasets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_many_datasets_response& v);
    static ores::dq::messaging::delete_many_datasets_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_dataset_versions_request& v);
    static ores::dq::messaging::list_dataset_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_dataset_versions_request& v);
    static ores::dq::messaging::list_dataset_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_dataset_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_dataset_versions_response& v);
    static ores::dq::messaging::list_dataset_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_dataset_versions_response& v);
    static ores::dq::messaging::list_dataset_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_version_request& v);
    static ores::dq::messaging::get_dataset_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_version_request& v);
    static ores::dq::messaging::get_dataset_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_dataset_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_dataset_version_response& v);
    static ores::dq::messaging::get_dataset_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_dataset_version_response& v);
    static ores::dq::messaging::get_dataset_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
