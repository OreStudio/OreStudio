/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"

namespace ores::qt {

using namespace ores::logging;

EntityListMdiWindow::EntityListMdiWindow(QWidget* parent)
    : QWidget(parent),
      pulseTimer_(new QTimer(this)) {
    connect(pulseTimer_, &QTimer::timeout, this, &EntityListMdiWindow::onPulseTimeout);
}

EntityListMdiWindow::~EntityListMdiWindow() = default;

void EntityListMdiWindow::initializeStaleIndicator(QAction* refreshAction,
                                                    const QString& iconPath) {
    refreshAction_ = refreshAction;

    // Store normal icon and create pulse (orange) icon
    normalReloadIcon_ = refreshAction_->icon();
    pulseReloadIcon_ = IconUtils::createRecoloredIcon(iconPath, color_constants::stale_indicator);

    // Set initial tooltip
    refreshAction_->setToolTip(normalRefreshTooltip());
}

void EntityListMdiWindow::markAsStale() {
    if (!refreshAction_) {
        BOOST_LOG_SEV(lg(), warn) << "markAsStale called but refreshAction not initialized";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Marking list as stale";
    refreshAction_->setToolTip(staleRefreshTooltip());
    startPulseAnimation();
}

void EntityListMdiWindow::clearStaleIndicator() {
    if (!refreshAction_) {
        return;
    }

    pulseTimer_->stop();
    pulseState_ = false;
    pulseCount_ = 0;
    refreshAction_->setIcon(normalReloadIcon_);
    refreshAction_->setToolTip(normalRefreshTooltip());
}

void EntityListMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start(500);
}

void EntityListMdiWindow::onPulseTimeout() {
    if (!refreshAction_) {
        pulseTimer_->stop();
        return;
    }

    pulseState_ = !pulseState_;
    refreshAction_->setIcon(pulseState_ ? pulseReloadIcon_ : normalReloadIcon_);

    ++pulseCount_;
    if (pulseCount_ >= 6) {
        pulseTimer_->stop();
        refreshAction_->setIcon(pulseReloadIcon_);
    }
}

}
