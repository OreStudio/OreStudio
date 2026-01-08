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
#include "ores.qt/FeatureFlagController.hpp"

#include <QMdiSubWindow>
#include "ores.qt/FeatureFlagMdiWindow.hpp"
#include "ores.qt/FeatureFlagDetailDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"

namespace ores::qt {

using namespace ores::telemetry::log;

namespace {
    // Event type name for feature flag changes
    constexpr std::string_view feature_flag_event_name =
        eventing::domain::event_traits<
            variability::eventing::feature_flags_changed_event>::name;
}

FeatureFlagController::FeatureFlagController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr),
      allDetachableWindows_(allDetachableWindows) {

    BOOST_LOG_SEV(lg(), debug) << "FeatureFlagController created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &FeatureFlagController::onNotificationReceived);

        // Subscribe to events when connected
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to feature flag change events";
            clientManager_->subscribeToEvent(std::string{feature_flag_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to feature flag change events after reconnect";
            clientManager_->subscribeToEvent(std::string{feature_flag_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to feature flag change events";
            clientManager_->subscribeToEvent(std::string{feature_flag_event_name});
        }
    }
}

FeatureFlagController::~FeatureFlagController() {
    BOOST_LOG_SEV(lg(), debug) << "FeatureFlagController destroyed";

    // Unsubscribe from feature flag change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from feature flag change events";
        clientManager_->unsubscribeFromEvent(std::string{feature_flag_event_name});
    }
}

void FeatureFlagController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "feature_flags");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing feature flags list window";
        return;
    }

    // Create new window
    listWindow_ = new FeatureFlagMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &FeatureFlagMdiWindow::statusChanged,
            this, &FeatureFlagController::statusMessage);
    connect(listWindow_, &FeatureFlagMdiWindow::errorOccurred,
            this, &FeatureFlagController::errorMessage);
    connect(listWindow_, &FeatureFlagMdiWindow::addNewRequested,
            this, &FeatureFlagController::onAddNewRequested);
    connect(listWindow_, &FeatureFlagMdiWindow::showFeatureFlagDetails,
            this, &FeatureFlagController::onShowDetails);
    connect(listWindow_, &FeatureFlagMdiWindow::featureFlagDeleted,
            this, &FeatureFlagController::onFeatureFlagDeleted);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Feature Flags");
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    allDetachableWindows_.append(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        allDetachableWindows_.removeOne(listMdiSubWindow_);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Feature flags list window created";
}

void FeatureFlagController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void FeatureFlagController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), debug) << "Add new feature flag requested";
    showDetailWindow({}, true);
}

void FeatureFlagController::onShowDetails(
    const variability::domain::feature_flags& flag) {
    BOOST_LOG_SEV(lg(), debug) << "Show details requested for: " << flag.name;
    showDetailWindow(flag, false);
}

void FeatureFlagController::showDetailWindow(
    const variability::domain::feature_flags& flag, bool createMode) {

    const QString identifier = createMode ? "new" :
        QString::fromStdString(flag.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window for: "
                                   << identifier.toStdString();
        return;
    }

    // Create detail dialog
    auto* detailDialog = new FeatureFlagDetailDialog();
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    if (createMode) {
        detailDialog->setFeatureFlag({});
        detailDialog->setCreateMode(true);
    } else {
        detailDialog->setFeatureFlag(flag);
    }

    // Connect signals
    connect(detailDialog, &FeatureFlagDetailDialog::statusMessage,
            this, &FeatureFlagController::statusMessage);
    connect(detailDialog, &FeatureFlagDetailDialog::errorMessage,
            this, &FeatureFlagController::errorMessage);
    connect(detailDialog, &FeatureFlagDetailDialog::featureFlagSaved,
            this, &FeatureFlagController::onFeatureFlagSaved);
    connect(detailDialog, &FeatureFlagDetailDialog::featureFlagDeleted,
            this, &FeatureFlagController::onFeatureFlagDeleted);

    // Create MDI subwindow
    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(detailDialog);

    const QString title = createMode ? "New Feature Flag" :
        QString("Feature Flag: %1").arg(QString::fromStdString(flag.name));
    subWindow->setWindowTitle(title);
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(detailDialog->sizeHint());

    mdiArea_->addSubWindow(subWindow);
    subWindow->show();

    // Track window
    track_window(key, subWindow);
    allDetachableWindows_.append(subWindow);

    // Cleanup when closed
    connect(subWindow, &QObject::destroyed, this, [this, key, subWindow]() {
        untrack_window(key);
        allDetachableWindows_.removeOne(subWindow);
    });

    BOOST_LOG_SEV(lg(), debug) << "Detail window created for: "
                               << identifier.toStdString();
}

void FeatureFlagController::onFeatureFlagSaved(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "Feature flag saved: " << name.toStdString();
    if (listWindow_) {
        listWindow_->markAsStale();
    }
}

void FeatureFlagController::onFeatureFlagDeleted(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "Feature flag deleted: " << name.toStdString();
    if (listWindow_) {
        listWindow_->markAsStale();
    }
}

void FeatureFlagController::refreshListWindow() {
    if (listWindow_) {
        BOOST_LOG_SEV(lg(), debug) << "Refreshing feature flags list";
        listWindow_->reload();
    }
}

void FeatureFlagController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {
    // Check if this is a feature flag change event
    if (eventType != QString::fromStdString(std::string{feature_flag_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received feature flag change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " flag names";

    // Mark the list window as stale if it's open
    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked feature flag window as stale";
    }
}

}
