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
#include "ores.qt/CountryController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CountryDetailDialog.hpp"
#include "ores.qt/CountryHistoryDialog.hpp"
#include "ores.qt/CountryMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/country_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view country_event_name =
    eventing::domain::event_traits<refdata::eventing::country_changed_event>::name;
}

CountryController::CountryController(QMainWindow* mainWindow,
                                     QMdiArea* mdiArea,
                                     ClientManager* clientManager,
                                     const QString& username,
                                     QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, country_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CountryController created";
}

void CountryController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "countries");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CountryMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &CountryMdiWindow::statusChanged, this, &CountryController::statusMessage);
    connect(listWindow_, &CountryMdiWindow::errorOccurred, this, &CountryController::errorMessage);
    connect(listWindow_,
            &CountryMdiWindow::showCountryDetails,
            this,
            &CountryController::onShowDetails);
    connect(listWindow_,
            &CountryMdiWindow::addNewRequested,
            this,
            &CountryController::onAddNewRequested);
    connect(listWindow_,
            &CountryMdiWindow::showCountryHistory,
            this,
            &CountryController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Countries");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Globe, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<CountryController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Country list window created";
}

void CountryController::closeAllWindows() {
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

void CountryController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CountryController::onShowDetails(const refdata::domain::country& country) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << country.alpha2_code;
    showDetailWindow(country);
}

void CountryController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new country requested";
    showAddWindow();
}

void CountryController::onShowHistory(const refdata::domain::country& country) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << country.alpha2_code;
    showHistoryWindow(QString::fromStdString(country.alpha2_code));
}

void CountryController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new country";

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(
        detailDialog, &CountryDetailDialog::statusMessage, this, &CountryController::statusMessage);
    connect(
        detailDialog, &CountryDetailDialog::errorMessage, this, &CountryController::errorMessage);
    connect(detailDialog,
            &CountryDetailDialog::countrySaved,
            this,
            [self = QPointer<CountryController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Country saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Country");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Globe, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CountryController::showDetailWindow(const refdata::domain::country& country) {

    const QString identifier = QString::fromStdString(country.alpha2_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << country.alpha2_code;

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCountry(country);

    connect(
        detailDialog, &CountryDetailDialog::statusMessage, this, &CountryController::statusMessage);
    connect(
        detailDialog, &CountryDetailDialog::errorMessage, this, &CountryController::errorMessage);
    connect(detailDialog,
            &CountryDetailDialog::countrySaved,
            this,
            [self = QPointer<CountryController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Country saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CountryDetailDialog::countryDeleted,
            this,
            [self = QPointer<CountryController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Country deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Country: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Globe, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, detailWindow);

    QPointer<CountryController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CountryController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for country: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new CountryHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CountryHistoryDialog::statusChanged,
            this,
            [self = QPointer<CountryController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CountryHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CountryController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CountryHistoryDialog::revertVersionRequested,
            this,
            &CountryController::onRevertVersion);
    connect(historyDialog,
            &CountryHistoryDialog::openVersionRequested,
            this,
            &CountryController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Country History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);
    UiPersistence::restoreMdiGeometry(windowKey, historyWindow);

    QPointer<CountryController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CountryController::onOpenVersion(const refdata::domain::country& country, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for country: " << country.alpha2_code;

    const QString code = QString::fromStdString(country.alpha2_code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CountryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCountry(country);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CountryDetailDialog::statusMessage,
            this,
            [self = QPointer<CountryController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CountryDetailDialog::errorMessage,
            this,
            [self = QPointer<CountryController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Country: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CountryController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CountryController::onRevertVersion(const refdata::domain::country& country) {
    BOOST_LOG_SEV(lg(), info) << "Reverting country to version: " << country.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CountryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCountry(country);
    detailDialog->setCreateMode(false);

    connect(
        detailDialog, &CountryDetailDialog::statusMessage, this, &CountryController::statusMessage);
    connect(
        detailDialog, &CountryDetailDialog::errorMessage, this, &CountryController::errorMessage);
    connect(detailDialog,
            &CountryDetailDialog::countrySaved,
            this,
            [self = QPointer<CountryController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Country reverted: " << code.toStdString();
                emit self->statusMessage(QString("Country '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Country: %1").arg(QString::fromStdString(country.alpha2_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CountryController::listWindow() const {
    return listWindow_;
}

}
