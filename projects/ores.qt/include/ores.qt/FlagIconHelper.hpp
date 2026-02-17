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
#ifndef ORES_QT_FLAG_ICON_HELPER_HPP
#define ORES_QT_FLAG_ICON_HELPER_HPP

#include <QAction>
#include <QComboBox>
#include <QLineEdit>

namespace ores::qt {

/**
 * @brief Set a leading flag icon on a QLineEdit.
 *
 * Manages the QAction lifecycle: removes any previous action stored in
 * @p action_ptr before inserting the new one.
 *
 * @param edit      The line edit to decorate
 * @param icon      The icon to display (empty icon removes the action)
 * @param action_ptr Reference to the owning QAction pointer (managed externally)
 */
inline void set_line_edit_flag_icon(
    QLineEdit* edit, const QIcon& icon, QAction*& action_ptr) {
    if (action_ptr) {
        edit->removeAction(action_ptr);
        delete action_ptr;
        action_ptr = nullptr;
    }
    if (!icon.isNull()) {
        action_ptr = edit->addAction(icon, QLineEdit::LeadingPosition);
    }
}

/**
 * @brief Set flag icons on every item in a QComboBox.
 *
 * Iterates each item, passes its text to @p resolver to obtain a QIcon,
 * and calls setItemIcon() with the result.
 *
 * @tparam Resolver  Callable with signature QIcon(const std::string&)
 * @param combo      The combo box to decorate
 * @param resolver   Callable that maps an item's text to an icon
 */
template<typename Resolver>
void set_combo_flag_icons(QComboBox* combo, Resolver&& resolver) {
    for (int i = 0; i < combo->count(); ++i) {
        const std::string code = combo->itemText(i).toStdString();
        QIcon icon = resolver(code);
        combo->setItemIcon(i, icon);
    }
}

}

#endif
