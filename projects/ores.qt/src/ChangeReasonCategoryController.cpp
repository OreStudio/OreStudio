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
#include "ores.qt/ChangeReasonCategoryController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ChangeReasonCategoryMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.iam/eventing/change_reason_category_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    // Event type name for change reason category changes
    constexpr std::string_view category_event_name =
        eventing::domain::event_traits<
            iam::eventing::change_reason_category_changed_event>::name;
}

ChangeReasonCategoryController::ChangeReasonCategoryController(
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

    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonCategoryController created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &ChangeReasonCategoryController::onNotificationReceived);

        // Subscribe to events when connected
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to category change events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to category change events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        }
    }
}

ChangeReasonCategoryController::~ChangeReasonCategoryController() {
    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonCategoryController destroyed";

    // Unsubscribe from events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from category change events";
        clientManager_->unsubscribeFromEvent(std::string{category_event_name});
    }
}

void ChangeReasonCategoryController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "change_reason_categories");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ChangeReasonCategoryMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::statusChanged,
            this, &ChangeReasonCategoryController::statusMessage);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::errorOccurred,
            this, &ChangeReasonCategoryController::errorMessage);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::showCategoryDetails,
            this, &ChangeReasonCategoryController::onShowDetails);

    // Create MDI subwindow
    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Change Reason Categories");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));
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

    BOOST_LOG_SEV(lg(), debug) << "Category list window created";
}

void ChangeReasonCategoryController::closeAllWindows() {
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

void ChangeReasonCategoryController::onShowDetails(
    const iam::domain::change_reason_category& category) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << category.code;
    showDetailWindow(category);
}

void ChangeReasonCategoryController::showDetailWindow(
    const iam::domain::change_reason_category& category) {

    const QString identifier = QString::fromStdString(category.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    // For now, show a simple message box with category details
    // Full detail dialog can be added later
    QString details = QString(
        "<h3>Change Reason Category</h3>"
        "<p><b>Code:</b> %1</p>"
        "<p><b>Description:</b> %2</p>"
        "<p><b>Version:</b> %3</p>"
        "<p><b>Recorded By:</b> %4</p>"
        "<p><b>Commentary:</b> %5</p>")
        .arg(QString::fromStdString(category.code))
        .arg(QString::fromStdString(category.description))
        .arg(category.version)
        .arg(QString::fromStdString(category.recorded_by))
        .arg(QString::fromStdString(category.change_commentary));

    QMessageBox::information(mainWindow_, "Category Details", details);
}

void ChangeReasonCategoryController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    if (eventType != QString::fromStdString(std::string{category_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received category change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " category codes";

    // Mark the list window as stale
    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked category list as stale";
    }
}

}
