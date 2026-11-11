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
#ifndef ORES_HTTP_ROUTES_IAM_ACCOUNT_VIEW_HPP
#define ORES_HTTP_ROUTES_IAM_ACCOUNT_VIEW_HPP

#include "ores.iam.api/domain/account.hpp"
#include <optional>
#include <string>

/**
 * @brief An account as the access screens read it.
 *
 * The BFF translated each account the service sent into the browser's own type:
 * camelCase names, an unknown account type read as a person, and the nil
 * identifier on a reference read as no reference. The routes that answer with
 * an account answer it this way, so the screens read the same object from
 * either server.
 */
namespace ores::http::routes::iam::account_view {

/**
 * @brief One account, as a JSON object.
 */
[[nodiscard]] std::string json_of(const ores::iam::domain::account& account);

/**
 * @brief One account, or null when there is none.
 */
[[nodiscard]] std::string json_of(const std::optional<ores::iam::domain::account>& account);

}

#endif
