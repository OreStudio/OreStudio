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
#ifndef ORES_QT_BINDING_MODE_LABEL_HPP
#define ORES_QT_BINDING_MODE_LABEL_HPP

#include "ores.synthetic.api/domain/binding_mode.hpp"
#include <QCoreApplication>
#include <QString>

namespace ores::qt {

/**
 * @brief Human-readable label for a market_data_generation_config's binding mode.
 */
inline QString bindingModeLabel(const ores::synthetic::domain::binding_mode& bindingMode) {
    using ores::synthetic::domain::binding_mode;
    switch (bindingMode) {
        case binding_mode::bound:
            return QCoreApplication::translate("BindingModeLabel", "Bound");
        case binding_mode::sandboxed:
            return QCoreApplication::translate("BindingModeLabel", "Sandboxed");
    }
    return QCoreApplication::translate("BindingModeLabel", "Unknown");
}

}

#endif
