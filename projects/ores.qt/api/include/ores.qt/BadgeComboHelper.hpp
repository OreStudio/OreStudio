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
#ifndef ORES_QT_BADGE_COMBO_HELPER_HPP
#define ORES_QT_BADGE_COMBO_HELPER_HPP

#include "ores.qt/export.hpp"
#include <QComboBox>
#include <QObject>
#include <string>

namespace ores::qt {

class BadgeCache;

/**
 * @brief Render every item in a combo box as a badge, using the
 * database-driven badge system — counterpart to
 * FlagIconHelper::apply_flag_icons for badges instead of flags.
 *
 * Installs an item delegate that paints each row (popup and the current
 * selection alike) as a left-aligned badge pill via
 * DelegatePaintUtils::draw_inline_badge() — the same badge rendering
 * EntityItemDelegate uses for grid columns, just left- rather than
 * centre-anchored to suit a combo row — resolving each badge from
 * (@p badge_key, item text) at paint time so the combo always reflects
 * the live cache.
 *
 * @param combo     The combo box to decorate (no-op if null).
 * @param cache     The badge cache (no-op if null); painting falls back
 * to the same neutral badge EntityItemDelegate uses when a value has no
 * resolvable definition.
 * @param badge_key The code_domain to resolve items against.
 */
ORES_QT_API void
apply_combo_badges(QComboBox* combo, BadgeCache* cache, const std::string& badge_key);

/**
 * @brief Wire up a combo box to render its items as badges — currently
 * just apply_combo_badges(), kept as a distinct entry point (mirroring
 * setup_flag_combo()'s shape) in case a future live-refresh signal needs
 * reconnecting here, the way flags reconnect on ImageCache::allLoaded.
 *
 * @param context   The QObject whose lifetime governs the delegate.
 * @param combo     The combo box to decorate (no-op if null).
 * @param cache     The badge cache (no-op if null).
 * @param badge_key The code_domain to resolve items against.
 */
ORES_QT_API void setup_badge_combo(QObject* context,
                                   QComboBox* combo,
                                   BadgeCache* cache,
                                   const std::string& badge_key);

}

#endif
