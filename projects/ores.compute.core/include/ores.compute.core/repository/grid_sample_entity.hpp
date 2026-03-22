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
#ifndef ORES_COMPUTE_REPOSITORY_GRID_SAMPLE_ENTITY_HPP
#define ORES_COMPUTE_REPOSITORY_GRID_SAMPLE_ENTITY_HPP

#include <string>
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

/**
 * @brief sqlgen entity for ores_compute_grid_samples_tbl.
 */
struct grid_sample_entity {
    constexpr static const char* schema    = "public";
    constexpr static const char* tablename = "ores_compute_grid_samples_tbl";

    sqlgen::PrimaryKey<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> sampled_at;
    sqlgen::PrimaryKey<std::string> tenant_id;

    int total_hosts{0};
    int online_hosts{0};
    int idle_hosts{0};

    int results_inactive{0};
    int results_unsent{0};
    int results_in_progress{0};
    int results_done{0};

    int total_workunits{0};
    int total_batches{0};
    int active_batches{0};

    int outcomes_success{0};
    int outcomes_client_error{0};
    int outcomes_no_reply{0};
};

}

#endif
