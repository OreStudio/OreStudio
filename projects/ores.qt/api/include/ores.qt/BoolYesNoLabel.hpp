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
#ifndef ORES_QT_BOOL_YES_NO_LABEL_HPP
#define ORES_QT_BOOL_YES_NO_LABEL_HPP

#include <QCoreApplication>
#include <QString>
#include <optional>

namespace ores::qt {

/**
 * @brief "Yes"/"No" label for an integer-backed boolean flag column.
 *
 * Domain models often store boolean flags as int (e.g. 0/1) rather than
 * a native bool column type. Pair with an is_badge column whose
 * badge_key maps "Yes"/"No" to colours via the database-driven badge
 * system (BadgeCache) — this formatter only produces the display text
 * the badge resolver keys off.
 */
inline QString boolYesNoLabel(int value) {
    return value != 0 ? QCoreApplication::translate("BoolYesNoLabel", "Yes") :
                        QCoreApplication::translate("BoolYesNoLabel", "No");
}

/**
 * @brief Overload for a nullable boolean column (e.g.
 * currency_pair_convention.spot_relative/end_of_month); an unset value
 * reads as "No".
 */
inline QString boolYesNoLabel(std::optional<bool> value) {
    return boolYesNoLabel(value.value_or(false) ? 1 : 0);
}

}

#endif
