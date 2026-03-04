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
#include "ores.mq/messaging/mq_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::mq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ---------------------------------------------------------------------------
// queue_definition wire helpers
// ---------------------------------------------------------------------------

void write_queue_definition(std::vector<std::byte>& buffer,
    const domain::queue_definition& qd) {
    writer::write_string(buffer, boost::uuids::to_string(qd.id));

    // tenant_id (optional)
    if (qd.tenant_id) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer, boost::uuids::to_string(*qd.tenant_id));
    } else {
        writer::write_bool(buffer, false);
    }

    // party_id (optional)
    if (qd.party_id) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer, boost::uuids::to_string(*qd.party_id));
    } else {
        writer::write_bool(buffer, false);
    }

    // scope_type as string
    std::string scope_str;
    switch (qd.scope_type) {
    case domain::queue_scope_type::tenant: scope_str = "tenant"; break;
    case domain::queue_scope_type::system: scope_str = "system"; break;
    default:                               scope_str = "party";  break;
    }
    writer::write_string(buffer, scope_str);

    // queue_type as string
    std::string qtype_str = (qd.type == domain::queue_type::channel)
        ? "channel" : "task";
    writer::write_string(buffer, qtype_str);

    writer::write_string(buffer, qd.name);
    writer::write_string(buffer, qd.description);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point_utc(qd.created_at));
    writer::write_bool(buffer, qd.is_active);
}

std::expected<domain::queue_definition, error_code>
read_queue_definition(std::span<const std::byte>& data) {
    domain::queue_definition qd;

    auto id_result = reader::read_string(data);
    if (!id_result) return std::unexpected(id_result.error());
    try {
        qd.id = boost::lexical_cast<boost::uuids::uuid>(*id_result);
    } catch (...) {
        return std::unexpected(error_code::invalid_request);
    }

    // tenant_id
    auto has_tenant = reader::read_bool(data);
    if (!has_tenant) return std::unexpected(has_tenant.error());
    if (*has_tenant) {
        auto tid_result = reader::read_string(data);
        if (!tid_result) return std::unexpected(tid_result.error());
        try {
            qd.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*tid_result);
        } catch (...) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    // party_id
    auto has_party = reader::read_bool(data);
    if (!has_party) return std::unexpected(has_party.error());
    if (*has_party) {
        auto pid_result = reader::read_string(data);
        if (!pid_result) return std::unexpected(pid_result.error());
        try {
            qd.party_id = boost::lexical_cast<boost::uuids::uuid>(*pid_result);
        } catch (...) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    // scope_type
    auto scope_result = reader::read_string(data);
    if (!scope_result) return std::unexpected(scope_result.error());
    if (*scope_result == "tenant")
        qd.scope_type = domain::queue_scope_type::tenant;
    else if (*scope_result == "system")
        qd.scope_type = domain::queue_scope_type::system;
    else
        qd.scope_type = domain::queue_scope_type::party;

    // queue_type
    auto qtype_result = reader::read_string(data);
    if (!qtype_result) return std::unexpected(qtype_result.error());
    qd.type = (*qtype_result == "channel")
        ? domain::queue_type::channel : domain::queue_type::task;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    qd.name = *name_result;

    auto desc_result = reader::read_string(data);
    if (!desc_result) return std::unexpected(desc_result.error());
    qd.description = *desc_result;

    auto ts_result = reader::read_string(data);
    if (!ts_result) return std::unexpected(ts_result.error());
    try {
        qd.created_at = ores::platform::time::datetime::parse_time_point(*ts_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto active_result = reader::read_bool(data);
    if (!active_result) return std::unexpected(active_result.error());
    qd.is_active = *active_result;

    return qd;
}

// ---------------------------------------------------------------------------
// queue_stats wire helpers
// ---------------------------------------------------------------------------

void write_queue_stats(std::vector<std::byte>& buffer,
    const domain::queue_stats& qs) {
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point_utc(qs.recorded_at));
    writer::write_string(buffer, boost::uuids::to_string(qs.queue_id));

    if (qs.tenant_id) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer, boost::uuids::to_string(*qs.tenant_id));
    } else {
        writer::write_bool(buffer, false);
    }

    if (qs.party_id) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer, boost::uuids::to_string(*qs.party_id));
    } else {
        writer::write_bool(buffer, false);
    }

    writer::write_int64(buffer, qs.pending_count);
    writer::write_int64(buffer, qs.processing_count);
    writer::write_int64(buffer, qs.total_archived);
}

std::expected<domain::queue_stats, error_code>
read_queue_stats(std::span<const std::byte>& data) {
    domain::queue_stats qs;

    auto ts_result = reader::read_string(data);
    if (!ts_result) return std::unexpected(ts_result.error());
    try {
        qs.recorded_at = ores::platform::time::datetime::parse_time_point(*ts_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    try {
        qs.queue_id = boost::lexical_cast<boost::uuids::uuid>(*qid_result);
    } catch (...) {
        return std::unexpected(error_code::invalid_request);
    }

    auto has_tenant = reader::read_bool(data);
    if (!has_tenant) return std::unexpected(has_tenant.error());
    if (*has_tenant) {
        auto tid_result = reader::read_string(data);
        if (!tid_result) return std::unexpected(tid_result.error());
        try {
            qs.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*tid_result);
        } catch (...) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto has_party = reader::read_bool(data);
    if (!has_party) return std::unexpected(has_party.error());
    if (*has_party) {
        auto pid_result = reader::read_string(data);
        if (!pid_result) return std::unexpected(pid_result.error());
        try {
            qs.party_id = boost::lexical_cast<boost::uuids::uuid>(*pid_result);
        } catch (...) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto pending_result = reader::read_int64(data);
    if (!pending_result) return std::unexpected(pending_result.error());
    qs.pending_count = *pending_result;

    auto processing_result = reader::read_int64(data);
    if (!processing_result) return std::unexpected(processing_result.error());
    qs.processing_count = *processing_result;

    auto archived_result = reader::read_int64(data);
    if (!archived_result) return std::unexpected(archived_result.error());
    qs.total_archived = *archived_result;

    return qs;
}

// ---------------------------------------------------------------------------
// queue_message wire helpers
// ---------------------------------------------------------------------------

void write_queue_message(std::vector<std::byte>& buffer,
    const queue_message& m) {
    writer::write_int64(buffer, m.msg_id);
    writer::write_string(buffer, m.queue_id);
    writer::write_string(buffer, m.message_type);
    writer::write_string(buffer, m.payload_type);
    writer::write_string(buffer, m.status);
    writer::write_string(buffer, m.created_at);
    writer::write_string(buffer, m.visible_after);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(m.read_count));
    writer::write_string(buffer, m.payload);
}

std::expected<queue_message, error_code>
read_queue_message(std::span<const std::byte>& data) {
    queue_message m;

    auto id_result = reader::read_int64(data);
    if (!id_result) return std::unexpected(id_result.error());
    m.msg_id = *id_result;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    m.queue_id = *qid_result;

    auto mtype_result = reader::read_string(data);
    if (!mtype_result) return std::unexpected(mtype_result.error());
    m.message_type = *mtype_result;

    auto ptype_result = reader::read_string(data);
    if (!ptype_result) return std::unexpected(ptype_result.error());
    m.payload_type = *ptype_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    m.status = *status_result;

    auto created_result = reader::read_string(data);
    if (!created_result) return std::unexpected(created_result.error());
    m.created_at = *created_result;

    auto vt_result = reader::read_string(data);
    if (!vt_result) return std::unexpected(vt_result.error());
    m.visible_after = *vt_result;

    auto rct_result = reader::read_uint32(data);
    if (!rct_result) return std::unexpected(rct_result.error());
    m.read_count = static_cast<std::int32_t>(*rct_result);

    auto payload_result = reader::read_string(data);
    if (!payload_result) return std::unexpected(payload_result.error());
    m.payload = *payload_result;

    return m;
}

std::expected<std::vector<queue_message>, error_code>
read_queue_messages(std::span<const std::byte>& data) {
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    std::vector<queue_message> messages;
    messages.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_message(data);
        if (!result) return std::unexpected(result.error());
        messages.push_back(std::move(*result));
    }
    return messages;
}

} // anonymous namespace

// ============================================================================
// get_queues_request
// ============================================================================

std::vector<std::byte> get_queues_request::serialize() const {
    return {};
}

std::expected<get_queues_request, error_code>
get_queues_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_queues_request{};
}

std::ostream& operator<<(std::ostream& s, const get_queues_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queues_response
// ============================================================================

std::vector<std::byte> get_queues_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(queues.size()));
    for (const auto& qd : queues)
        write_queue_definition(buffer, qd);
    return buffer;
}

std::expected<get_queues_response, error_code>
get_queues_response::deserialize(std::span<const std::byte> data) {
    get_queues_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.queues.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_definition(data);
        if (!result) return std::unexpected(result.error());
        response.queues.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_queues_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_stats_request
// ============================================================================

std::vector<std::byte> get_queue_stats_request::serialize() const {
    return {};
}

std::expected<get_queue_stats_request, error_code>
get_queue_stats_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_queue_stats_request{};
}

std::ostream& operator<<(std::ostream& s, const get_queue_stats_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_stats_response
// ============================================================================

std::vector<std::byte> get_queue_stats_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(stats.size()));
    for (const auto& qs : stats)
        write_queue_stats(buffer, qs);
    return buffer;
}

std::expected<get_queue_stats_response, error_code>
get_queue_stats_response::deserialize(std::span<const std::byte> data) {
    get_queue_stats_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.stats.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_stats(data);
        if (!result) return std::unexpected(result.error());
        response.stats.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_queue_stats_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_stats_samples_request
// ============================================================================

std::vector<std::byte> get_queue_stats_samples_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_id);
    if (from) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point_utc(*from));
    } else {
        writer::write_bool(buffer, false);
    }
    if (to) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point_utc(*to));
    } else {
        writer::write_bool(buffer, false);
    }
    return buffer;
}

std::expected<get_queue_stats_samples_request, error_code>
get_queue_stats_samples_request::deserialize(std::span<const std::byte> data) {
    get_queue_stats_samples_request request;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    request.queue_id = *qid_result;

    auto has_from_result = reader::read_bool(data);
    if (!has_from_result) return std::unexpected(has_from_result.error());
    if (*has_from_result) {
        auto ts_result = reader::read_string(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        try {
            request.from = ores::platform::time::datetime::parse_time_point(*ts_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto has_to_result = reader::read_bool(data);
    if (!has_to_result) return std::unexpected(has_to_result.error());
    if (*has_to_result) {
        auto ts_result = reader::read_string(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        try {
            request.to = ores::platform::time::datetime::parse_time_point(*ts_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    return request;
}

std::ostream& operator<<(std::ostream& s,
    const get_queue_stats_samples_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_stats_samples_response
// ============================================================================

std::vector<std::byte> get_queue_stats_samples_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_string(buffer, queue_id);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(samples.size()));
    for (const auto& qs : samples)
        write_queue_stats(buffer, qs);
    return buffer;
}

std::expected<get_queue_stats_samples_response, error_code>
get_queue_stats_samples_response::deserialize(std::span<const std::byte> data) {
    get_queue_stats_samples_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    response.queue_id = *qid_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.samples.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_stats(data);
        if (!result) return std::unexpected(result.error());
        response.samples.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s,
    const get_queue_stats_samples_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// create_queue_request
// ============================================================================

std::vector<std::byte> create_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_string(buffer, scope_type);
    writer::write_string(buffer, queue_type);
    writer::write_string(buffer, description);
    return buffer;
}

std::expected<create_queue_request, error_code>
create_queue_request::deserialize(std::span<const std::byte> data) {
    create_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto scope_result = reader::read_string(data);
    if (!scope_result) return std::unexpected(scope_result.error());
    request.scope_type = *scope_result;

    auto qtype_result = reader::read_string(data);
    if (!qtype_result) return std::unexpected(qtype_result.error());
    request.queue_type = *qtype_result;

    auto desc_result = reader::read_string(data);
    if (!desc_result) return std::unexpected(desc_result.error());
    request.description = *desc_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const create_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// create_queue_response
// ============================================================================

std::vector<std::byte> create_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_string(buffer, queue_id);
    return buffer;
}

std::expected<create_queue_response, error_code>
create_queue_response::deserialize(std::span<const std::byte> data) {
    create_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    response.queue_id = *qid_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const create_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// drop_queue_request
// ============================================================================

std::vector<std::byte> drop_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    return buffer;
}

std::expected<drop_queue_request, error_code>
drop_queue_request::deserialize(std::span<const std::byte> data) {
    drop_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const drop_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// drop_queue_response
// ============================================================================

std::vector<std::byte> drop_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<drop_queue_response, error_code>
drop_queue_response::deserialize(std::span<const std::byte> data) {
    drop_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const drop_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// purge_queue_request
// ============================================================================

std::vector<std::byte> purge_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    return buffer;
}

std::expected<purge_queue_request, error_code>
purge_queue_request::deserialize(std::span<const std::byte> data) {
    purge_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const purge_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// purge_queue_response
// ============================================================================

std::vector<std::byte> purge_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_int64(buffer, purged_count);
    return buffer;
}

std::expected<purge_queue_response, error_code>
purge_queue_response::deserialize(std::span<const std::byte> data) {
    purge_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_int64(data);
    if (!count_result) return std::unexpected(count_result.error());
    response.purged_count = *count_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const purge_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// send_message_request
// ============================================================================

std::vector<std::byte> send_message_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_id);
    writer::write_string(buffer, message_type);
    writer::write_string(buffer, payload);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(delay_seconds));
    return buffer;
}

std::expected<send_message_request, error_code>
send_message_request::deserialize(std::span<const std::byte> data) {
    send_message_request request;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    request.queue_id = *qid_result;

    auto mtype_result = reader::read_string(data);
    if (!mtype_result) return std::unexpected(mtype_result.error());
    request.message_type = *mtype_result;

    auto payload_result = reader::read_string(data);
    if (!payload_result) return std::unexpected(payload_result.error());
    request.payload = *payload_result;

    auto delay_result = reader::read_uint32(data);
    if (!delay_result) return std::unexpected(delay_result.error());
    request.delay_seconds = static_cast<std::int32_t>(*delay_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const send_message_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// send_message_response
// ============================================================================

std::vector<std::byte> send_message_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_int64(buffer, msg_id);
    return buffer;
}

std::expected<send_message_response, error_code>
send_message_response::deserialize(std::span<const std::byte> data) {
    send_message_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto id_result = reader::read_int64(data);
    if (!id_result) return std::unexpected(id_result.error());
    response.msg_id = *id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const send_message_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// read_messages_request
// ============================================================================

std::vector<std::byte> read_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_id);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(count));
    writer::write_uint32(buffer, static_cast<std::uint32_t>(vt_seconds));
    return buffer;
}

std::expected<read_messages_request, error_code>
read_messages_request::deserialize(std::span<const std::byte> data) {
    read_messages_request request;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    request.queue_id = *qid_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    request.count = static_cast<std::int32_t>(*count_result);

    auto vt_result = reader::read_uint32(data);
    if (!vt_result) return std::unexpected(vt_result.error());
    request.vt_seconds = static_cast<std::int32_t>(*vt_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const read_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// read_messages_response
// ============================================================================

std::vector<std::byte> read_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(messages.size()));
    for (const auto& m : messages)
        write_queue_message(buffer, m);
    return buffer;
}

std::expected<read_messages_response, error_code>
read_messages_response::deserialize(std::span<const std::byte> data) {
    read_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto msgs = read_queue_messages(data);
    if (!msgs) return std::unexpected(msgs.error());
    response.messages = std::move(*msgs);

    return response;
}

std::ostream& operator<<(std::ostream& s, const read_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// pop_messages_request
// ============================================================================

std::vector<std::byte> pop_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_id);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(count));
    return buffer;
}

std::expected<pop_messages_request, error_code>
pop_messages_request::deserialize(std::span<const std::byte> data) {
    pop_messages_request request;

    auto qid_result = reader::read_string(data);
    if (!qid_result) return std::unexpected(qid_result.error());
    request.queue_id = *qid_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    request.count = static_cast<std::int32_t>(*count_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const pop_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// pop_messages_response
// ============================================================================

std::vector<std::byte> pop_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(messages.size()));
    for (const auto& m : messages)
        write_queue_message(buffer, m);
    return buffer;
}

std::expected<pop_messages_response, error_code>
pop_messages_response::deserialize(std::span<const std::byte> data) {
    pop_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto msgs = read_queue_messages(data);
    if (!msgs) return std::unexpected(msgs.error());
    response.messages = std::move(*msgs);

    return response;
}

std::ostream& operator<<(std::ostream& s, const pop_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// ack_messages_request
// ============================================================================

std::vector<std::byte> ack_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(message_ids.size()));
    for (const auto id : message_ids)
        writer::write_int64(buffer, id);
    return buffer;
}

std::expected<ack_messages_request, error_code>
ack_messages_request::deserialize(std::span<const std::byte> data) {
    ack_messages_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.message_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_int64(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.message_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const ack_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// ack_messages_response
// ============================================================================

std::vector<std::byte> ack_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<ack_messages_response, error_code>
ack_messages_response::deserialize(std::span<const std::byte> data) {
    ack_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const ack_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// nack_message_request
// ============================================================================

std::vector<std::byte> nack_message_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_int64(buffer, message_id);
    writer::write_string(buffer, error);
    return buffer;
}

std::expected<nack_message_request, error_code>
nack_message_request::deserialize(std::span<const std::byte> data) {
    nack_message_request request;

    auto id_result = reader::read_int64(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.message_id = *id_result;

    auto error_result = reader::read_string(data);
    if (!error_result) return std::unexpected(error_result.error());
    request.error = *error_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const nack_message_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// nack_message_response
// ============================================================================

std::vector<std::byte> nack_message_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<nack_message_response, error_code>
nack_message_response::deserialize(std::span<const std::byte> data) {
    nack_message_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const nack_message_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// delete_messages_request (kept for backward compatibility)
// ============================================================================

std::vector<std::byte> delete_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(msg_ids.size()));
    for (const auto id : msg_ids)
        writer::write_int64(buffer, id);
    return buffer;
}

std::expected<delete_messages_request, error_code>
delete_messages_request::deserialize(std::span<const std::byte> data) {
    delete_messages_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.msg_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_int64(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.msg_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// delete_messages_response
// ============================================================================

std::vector<std::byte> delete_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(deleted_count));
    return buffer;
}

std::expected<delete_messages_response, error_code>
delete_messages_response::deserialize(std::span<const std::byte> data) {
    delete_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    response.deleted_count = static_cast<std::int32_t>(*count_result);

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
