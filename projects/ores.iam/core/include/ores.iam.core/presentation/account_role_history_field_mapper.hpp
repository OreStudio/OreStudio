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
 * Template: cpp_history_field_mapper.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_PRESENTATION_ACCOUNT_ROLE_HISTORY_FIELD_MAPPER_HPP
#define ORES_IAM_CORE_PRESENTATION_ACCOUNT_ROLE_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_role_repository.hpp"
#include <vector>

namespace ores::iam::presentation {

/**
 * @brief Renders one version of a account role to an ordered field
 * list for history-diff display.
 *
 * The first field, @c ID, names the link by both of its keys, so the versions
 * of one link pair up in a stream that holds many links. The last field before
 * the provenance is @c Closed At, empty while the link is open.
 */
[[nodiscard]] ORES_IAM_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_account_role_fields(const repository::account_role_history_row& v);

}

#endif
