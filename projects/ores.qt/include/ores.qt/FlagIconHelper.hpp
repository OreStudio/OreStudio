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
#include <QObject>

namespace ores::qt {

class ImageCache;

/**
 * @brief The type of flag icons to apply to a combo box.
 */
enum class FlagSource { Currency, Country, BusinessCentre };

/**
 * @brief Apply flag icons to a combo box using the given image cache.
 *
 * Dispatches to the appropriate ImageCache getter based on @p source.
 * For editable combos this updates the line-edit leading icon; for
 * non-editable combos this sets the item icons.
 *
 * @param combo   The combo box to decorate
 * @param cache   The image cache (may be null — no-op if so)
 * @param source  Which flag type to use
 */
void apply_flag_icons(QComboBox* combo, ImageCache* cache, FlagSource source);

/**
 * @brief Wire up a combo box for flag icons and keep them current.
 *
 * Calls apply_flag_icons() immediately, then arranges for icons to be
 * refreshed whenever:
 *  - ImageCache::allLoaded() fires (the full icon set has been loaded), and
 *  - The user changes the selection in an editable combo box.
 *
 * @param context  The QObject whose lifetime governs the connections
 * @param combo    The combo box to decorate
 * @param cache    The image cache (may be null — no-op if so)
 * @param source   Which flag type to use
 */
void setup_flag_combo(
    QObject* context, QComboBox* combo, ImageCache* cache, FlagSource source);

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
    // For non-editable combos, set item icons so they appear in both the
    // dropdown list and the closed-state button (the style renders the
    // selected item's icon there automatically).
    //
    // For editable combos, setting item icons causes the Qt Fusion style to
    // also render the selected item's icon beside the QLineEdit — in addition
    // to our leading QAction — producing two flags.  Use the leading QAction
    // exclusively for editable combos.
    if (!combo->isEditable()) {
        for (int i = 0; i < combo->count(); ++i) {
            const std::string code = combo->itemText(i).toStdString();
            combo->setItemIcon(i, resolver(code));
        }
    } else if (combo->lineEdit()) {
        update_combo_line_edit_icon(combo, resolver);
    }
    combo->update();
}

/**
 * @brief Update the leading icon on an editable QComboBox's line edit.
 *
 * Editable combo boxes don't show the current item's icon in the closed
 * display. This sets a QAction on the internal QLineEdit to show the
 * flag for the current selection.
 *
 * @tparam Resolver  Callable with signature QIcon(const std::string&)
 * @param combo      The editable combo box
 * @param resolver   Callable that maps text to an icon
 */
template<typename Resolver>
void update_combo_line_edit_icon(QComboBox* combo, Resolver&& resolver) {
    auto* lineEdit = combo->lineEdit();
    if (!lineEdit) return;

    // Remove any existing leading actions we added
    const auto actions = lineEdit->actions();
    for (auto* action : actions) {
        if (action->property("flag_icon").toBool()) {
            lineEdit->removeAction(action);
            delete action;
        }
    }

    const std::string code = combo->currentText().toStdString();
    if (!code.empty()) {
        QIcon icon = resolver(code);
        if (!icon.isNull()) {
            auto* action = lineEdit->addAction(icon, QLineEdit::LeadingPosition);
            action->setProperty("flag_icon", true);
        }
    }
}

}

#endif
