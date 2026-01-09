/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/service/session_converter.hpp"
#include "ores.platform/unreachable.hpp"

namespace ores::iam::service {

namespace {

/**
 * @brief Convert protocol enum from iam to comms representation.
 *
 * No default case so compiler warns if new enum values are added.
 */
comms::service::session_protocol
to_comms_protocol(domain::session_protocol p) {
    switch (p) {
        case domain::session_protocol::binary:
            return comms::service::session_protocol::binary;
        case domain::session_protocol::http:
            return comms::service::session_protocol::http;
    }
    platform::unreachable();
}

/**
 * @brief Convert protocol enum from comms to iam representation.
 *
 * No default case so compiler warns if new enum values are added.
 */
domain::session_protocol
from_comms_protocol(comms::service::session_protocol p) {
    switch (p) {
        case comms::service::session_protocol::binary:
            return domain::session_protocol::binary;
        case comms::service::session_protocol::http:
            return domain::session_protocol::http;
    }
    platform::unreachable();
}

}

std::shared_ptr<comms::service::session_data>
session_converter::to_session_data(const domain::session& s) {
    auto d = std::make_shared<comms::service::session_data>();
    d->id = s.id;
    d->account_id = s.account_id;
    d->start_time = s.start_time;
    d->end_time = s.end_time;
    d->client_ip = s.client_ip;
    d->client_identifier = s.client_identifier;
    d->client_version_major = s.client_version_major;
    d->client_version_minor = s.client_version_minor;
    d->bytes_sent = s.bytes_sent;
    d->bytes_received = s.bytes_received;
    d->country_code = s.country_code;
    d->protocol = to_comms_protocol(s.protocol);
    d->username = s.username;
    return d;
}

domain::session
session_converter::from_session_data(const comms::service::session_data& d) {
    domain::session s;
    s.id = d.id;
    s.account_id = d.account_id;
    s.start_time = d.start_time;
    s.end_time = d.end_time;
    s.client_ip = d.client_ip;
    s.client_identifier = d.client_identifier;
    s.client_version_major = d.client_version_major;
    s.client_version_minor = d.client_version_minor;
    s.bytes_sent = d.bytes_sent;
    s.bytes_received = d.bytes_received;
    s.country_code = d.country_code;
    s.protocol = from_comms_protocol(d.protocol);
    s.username = d.username;
    return s;
}

void session_converter::update_from_session_data(domain::session& s,
    const comms::service::session_data& d) {
    // Only update fields that can change during a session
    s.end_time = d.end_time;
    s.bytes_sent = d.bytes_sent;
    s.bytes_received = d.bytes_received;
}

}
