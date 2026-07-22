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
#ifndef ORES_IAM_CORE_PRESENTATION_ACCOUNT_CONTACT_INFORMATION_HISTORY_FIELD_MAPPER_HPP
#define ORES_IAM_CORE_PRESENTATION_ACCOUNT_CONTACT_INFORMATION_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.iam.core/export.hpp"
#include <vector>

namespace ores::iam::presentation {

/**
 * @brief Renders a account_contact_information to an ordered field list for
 * history-diff display. One line per field, in mapper order; no
 * runtime reflection.
 */
[[nodiscard]] ORES_IAM_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_account_contact_information_fields(const domain::account_contact_information& v);

}

#endif
