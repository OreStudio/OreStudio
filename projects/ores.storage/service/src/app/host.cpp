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
 * Template: cpp_service_app_host.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.storage.service/app/host.hpp"
#include "ores.service/service/host_runner.hpp"
#include "ores.storage.service/app/application.hpp"
#include "ores.storage.service/config/parser.hpp"

namespace ores::storage::service::app {

using ores::storage::service::config::parser;

boost::asio::awaitable<int> host::execute(const std::vector<std::string>& args,
                                          std::ostream& std_output,
                                          std::ostream& error_output,
                                          boost::asio::io_context& io_ctx) {
    return ores::service::service::run_host_async<parser, application>(
        args, std_output, error_output, io_ctx, lg());
}

}
