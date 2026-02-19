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

#include <QHeaderView>
#include <QMenu>
#include <QSettings>
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

void EntityListMdiWindow::closeEvent(QCloseEvent* event) {
    saveSettings();
    QWidget::closeEvent(event);
}

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

void EntityListMdiWindow::initializeTableSettings(
    QTableView* tableView,
    QAbstractItemModel* sourceModel,
    std::string_view settingsGroup,
    const QVector<int>& defaultHiddenColumns,
    const QSize& defaultSize,
    int settingsVersion) {

    settingsTableView_ = tableView;
    settingsModel_ = sourceModel;
    settingsGroup_ = QString::fromLatin1(settingsGroup.data(), settingsGroup.size());
    defaultHiddenColumns_ = defaultHiddenColumns;
    defaultSize_ = defaultSize;
    settingsVersion_ = settingsVersion;

    auto* header = settingsTableView_->horizontalHeader();
    header->setSectionResizeMode(QHeaderView::ResizeToContents);

    setupColumnVisibility();
    restoreTableSettings();
}

QSize EntityListMdiWindow::sizeHint() const {
    if (savedWindowSize_.isValid())
        return savedWindowSize_;
    return defaultSize_;
}

void EntityListMdiWindow::saveSettings() {
    if (!settingsTableView_)
        return;

    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup(settingsGroup_);

    auto* header = settingsTableView_->horizontalHeader();
    settings.setValue("headerState", header->saveState());
    settings.setValue("windowSize", size());

    settings.endGroup();
}

void EntityListMdiWindow::restoreTableSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup(settingsGroup_);

    auto* header = settingsTableView_->horizontalHeader();

    const int saved = settings.value("settingsVersion", 0).toInt();
    if (saved == settingsVersion_ && settings.contains("headerState")) {
        header->restoreState(settings.value("headerState").toByteArray());
    } else {
        settings.remove("headerState");
        settings.setValue("settingsVersion", settingsVersion_);
        for (int col : defaultHiddenColumns_) {
            header->setSectionHidden(col, true);
        }
    }

    // Always enforce ResizeToContents (guards against stale saved state)
    header->setSectionResizeMode(QHeaderView::ResizeToContents);

    if (settings.contains("windowSize"))
        savedWindowSize_ = settings.value("windowSize").toSize();

    settings.endGroup();
}

void EntityListMdiWindow::setupColumnVisibility() {
    auto* header = settingsTableView_->horizontalHeader();
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &EntityListMdiWindow::showHeaderContextMenu);

    connect(header, &QHeaderView::sectionMoved,
            this, &EntityListMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized,
            this, &EntityListMdiWindow::saveSettings);
}

void EntityListMdiWindow::showHeaderContextMenu(const QPoint& pos) {
    auto* header = settingsTableView_->horizontalHeader();
    QMenu menu(this);
    menu.setTitle(tr("Columns"));

    for (int col = 0; col < settingsModel_->columnCount(); ++col) {
        QString columnName = settingsModel_->headerData(
            col, Qt::Horizontal, Qt::DisplayRole).toString();

        QAction* action = menu.addAction(columnName);
        action->setCheckable(true);
        action->setChecked(!header->isSectionHidden(col));

        connect(action, &QAction::toggled, this,
                [this, header, col](bool visible) {
            header->setSectionHidden(col, !visible);
            saveSettings();
        });
    }

    menu.exec(header->mapToGlobal(pos));
}

}
