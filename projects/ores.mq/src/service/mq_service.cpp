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
#include "ores.mq/service/mq_service.hpp"

namespace ores::mq::service {

mq_service::mq_service(context ctx)
    : ctx_(std::move(ctx)) {}

boost::uuids::uuid mq_service::create_queue(
    const domain::queue_definition& def, const std::string& modified_by) {
    return queue_repo_.create_queue(ctx_, def, modified_by);
}

std::optional<domain::queue_definition> mq_service::find_queue(
    const std::string& name,
    const std::optional<boost::uuids::uuid>& tenant_id,
    const std::optional<boost::uuids::uuid>& party_id) {
    return queue_repo_.find_by_name(ctx_, name, tenant_id, party_id);
}

std::vector<domain::queue_definition> mq_service::list_queues() {
    return queue_repo_.list_active(ctx_);
}

void mq_service::deactivate(const boost::uuids::uuid& queue_id) {
    queue_repo_.deactivate(ctx_, queue_id);
}

std::int64_t mq_service::send(const boost::uuids::uuid& queue_id,
    const std::string& message_type, const std::string& payload,
    int delay_seconds) {
    return msg_repo_.send(ctx_, queue_id, message_type, payload, delay_seconds);
}

std::vector<domain::mq_message> mq_service::read(
    const boost::uuids::uuid& queue_id, int batch, int vt_seconds) {
    return msg_repo_.read(ctx_, queue_id, batch, vt_seconds);
}

void mq_service::ack(const std::vector<std::int64_t>& message_ids) {
    msg_repo_.ack(ctx_, message_ids);
}

void mq_service::nack(std::int64_t message_id, const std::string& error) {
    msg_repo_.nack(ctx_, message_id, error);
}

std::int64_t mq_service::purge(const boost::uuids::uuid& queue_id) {
    return msg_repo_.purge(ctx_, queue_id);
}

std::vector<domain::queue_stats> mq_service::get_stats() {
    return stats_repo_.read_latest(ctx_);
}

std::vector<domain::queue_stats> mq_service::get_stats_samples(
    const boost::uuids::uuid& queue_id,
    std::optional<std::chrono::system_clock::time_point> from,
    std::optional<std::chrono::system_clock::time_point> to) {
    return stats_repo_.read_samples(ctx_, queue_id, from, to);
}

}
