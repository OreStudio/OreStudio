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
#ifndef ORES_QT_COLOUR_SWATCH_HELPER_HPP
#define ORES_QT_COLOUR_SWATCH_HELPER_HPP

#include "ores.qt/export.hpp"
#include <QObject>
#include <QPushButton>
#include <QString>
#include <functional>

namespace ores::qt {

/**
 * @brief Paint @p button as a colour swatch for @p hex and store it as the
 * button's current value.
 *
 * Sets the button's background to @p hex via stylesheet, chooses a black or
 * white label colour for contrast, and shows @p hex itself as the button
 * text — used by codegen's `colour` detail-field kind for both initial
 * population (updateUiFromX) and after a picker choice, so a field's
 * current value is always readable directly off the widget via
 * colour_swatch_value(), with no separate member to keep in sync.
 *
 * @param button The swatch button to paint (no-op if null).
 * @param hex    A CSS colour string, e.g. "#22c55e". Invalid strings fall
 * back to white so the button never renders with the previous stale colour.
 */
ORES_QT_API void set_colour_swatch(QPushButton* button, const QString& hex);

/**
 * @brief Read the colour currently painted on @p button by set_colour_swatch().
 *
 * @param button The swatch button to read (returns "#ffffff" if null).
 */
ORES_QT_API QString colour_swatch_value(const QPushButton* button);

/**
 * @brief Wire a swatch button to open a QColorDialog on click and repaint
 * itself with the chosen colour.
 *
 * Mirrors setup_badge_combo()'s shape: a single entry point codegen calls
 * once per `colour` detail field, keeping the QColorDialog wiring itself
 * out of the generated .cpp.
 *
 * @param context    The QObject whose lifetime governs the connection.
 * @param button     The swatch button to wire (no-op if null).
 * @param on_changed Invoked after a valid colour is chosen and painted —
 * generated code passes onFieldChanged() here to mark the dialog dirty.
 */
ORES_QT_API void wire_colour_picker(QObject* context,
                                    QPushButton* button,
                                    const std::function<void()>& on_changed);

}

#endif
