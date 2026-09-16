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
#include "ores.shell/app/host.hpp"
#include "ores.service/service/host_runner_sync.hpp"
#include "ores.shell/app/application.hpp"
#include "ores.shell/config/parser.hpp"
#include <optional>

namespace ores::shell::app {

using ores::shell::config::parser;
using ores::service::service::run_host_sync;

int host::execute(const std::vector<std::string>& args,
                  std::ostream& std_output,
                  std::ostream& error_output) {
    const auto early_exit = [](const auto&) -> std::optional<int> {
        return std::nullopt;
    };

    const auto run_application = [](const auto& cfg) {
        ores::shell::app::application app(cfg.connection, cfg.login, cfg.script_path);
        app.run();
    };

    return run_host_sync<parser>(args, std_output, error_output, lg(), early_exit, run_application);
}

}
