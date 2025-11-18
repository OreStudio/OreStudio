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
#ifndef ORES_CLI_CONFIG_DELETE_OPTIONS_HPP
#define ORES_CLI_CONFIG_DELETE_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include "ores.cli/config/entity.hpp"

namespace ores::cli::config {

/**
 * @brief Configuration related to deleting entities.
 */
struct delete_options final {
    /**
     * @brief Which entity to delete.
     */
    entity target_entity;
    /**
     * @brief Key to identify the entity to delete (e.g., account ID/username).
     */
    std::string key;
};

std::ostream& operator<<(std::ostream& s, const delete_options& v);

}

#endif
