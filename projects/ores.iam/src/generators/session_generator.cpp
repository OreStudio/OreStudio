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
#include "ores.iam/generators/session_generator.hpp"

#include <atomic>
#include <boost/asio/ip/address.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::session generate_synthetic_session(
    utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;
    const auto tid = ctx.env().get_or(
        generation_keys::tenant_id, "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);

    domain::session r;
    r.tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();
    r.id = ctx.generate_uuid();
    r.account_id = ctx.generate_uuid();
    r.start_time = ctx.past_timepoint();
    r.client_ip = boost::asio::ip::make_address("127.0.0.1");
    r.client_identifier = "test-client-" + std::to_string(idx);
    r.client_version_major = 1;
    r.client_version_minor = 0;
    r.bytes_sent = 0;
    r.bytes_received = 0;
    r.country_code = "GB";
    r.protocol = domain::session_protocol::binary;
    r.username = "test_user_" + std::to_string(idx);
    return r;
}

std::vector<domain::session>
generate_synthetic_sessions(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::session> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i)
        r.push_back(generate_synthetic_session(ctx));
    return r;
}

}
