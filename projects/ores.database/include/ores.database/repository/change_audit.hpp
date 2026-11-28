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
#ifndef ORES_DATABASE_REPOSITORY_CHANGE_AUDIT_HPP
#define ORES_DATABASE_REPOSITORY_CHANGE_AUDIT_HPP

#include <string>

namespace ores::database::repository {

/**
 * @brief The change audit a row carries: who made the change, and why.
 *
 * Every temporal table holds the same four columns, and every domain type that
 * carries them states them as one =audit= group. This is the row's own type for
 * that group, so a model that declares the group generates its entity with one
 * member here rather than four members of its own.
 *
 * The transaction-time window is not part of it. =valid_from= and =valid_to=
 * belong to the store, are read in the database's own timestamp type, and the
 * domain states them as the single instant the version was recorded at.
 */
struct change_audit final {
    /**
     * @brief Username of the system actor that wrote this row.
     */
    std::string modified_by;

    /**
     * @brief Username of the business user on whose behalf the change was made.
     */
    std::string performed_by;

    /**
     * @brief Structured reason code for the change, from the =dq= vocabulary.
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary describing the reason for the change.
     */
    std::string change_commentary;
};

}

#endif
