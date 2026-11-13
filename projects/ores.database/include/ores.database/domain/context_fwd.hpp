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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_DOMAIN_CONTEXT_FWD_HPP
#define ORES_DATABASE_DOMAIN_CONTEXT_FWD_HPP

namespace ores::database {

/**
 * @brief The per-request database context.
 *
 * A header that only names a context includes this file and not
 * =ores.database/domain/context.hpp=, so it does not parse libpq, the pool, the
 * clock or the codec headers that the definition needs. A reference, a pointer,
 * or a smart pointer to a context needs nothing more than this declaration.
 */
class context;

}

#endif
