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
#include "ores.http/routes/reporting/reporting_routes.hpp"
#include "ores.http/routes/reporting/choice_routes.hpp"
#include "ores.http/routes/reporting/configuration_routes.hpp"
#include "ores.http/routes/reporting/cut_kind_routes.hpp"
#include "ores.http/routes/reporting/cut_routes.hpp"
#include "ores.http/routes/reporting/report_routes.hpp"
#include "ores.http/routes/reporting/result_routes.hpp"
#include "ores.http/routes/reporting/run_file_routes.hpp"
#include "ores.http/routes/reporting/run_routes.hpp"

namespace ores::http::routes::reporting {

void reporting_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session,
    std::string storage_dir) {
    choice_routes::register_routes(router, registry, session);
    configuration_routes::register_routes(router, registry, session);
    cut_kind_routes::register_routes(router, registry, session);
    cut_routes::register_routes(router, registry, session);
    report_routes::register_routes(router, registry, session);
    result_routes::register_routes(router, registry, session);
    run_file_routes::register_routes(router, registry, session, std::move(storage_dir));
    run_routes::register_routes(router, registry, session);
}

}
