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
#include "ores.iam/generators/login_info_generator.hpp"

#include <boost/asio/ip/address.hpp>
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::iam::generators {

using ores::utility::generation::generation_keys;

domain::login_info generate_synthetic_login_info(
    utility::generation::generation_context& ctx) {
    const auto tid = ctx.env().get_or(
        generation_keys::tenant_id, "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);

    domain::login_info r;
    r.tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();
    r.last_login = ctx.past_timepoint();
    r.account_id = ctx.generate_uuid();
    r.failed_logins = 0;
    r.locked = false;
    r.online = false;
    r.password_reset_required = false;
    r.last_ip = boost::asio::ip::make_address("127.0.0.1");
    r.last_attempt_ip = boost::asio::ip::make_address("127.0.0.1");
    return r;
}

std::vector<domain::login_info>
generate_synthetic_login_infos(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::login_info> r;
    r.reserve(n);
    for (std::size_t i = 0; i < n; ++i)
        r.push_back(generate_synthetic_login_info(ctx));
    return r;
}

}
