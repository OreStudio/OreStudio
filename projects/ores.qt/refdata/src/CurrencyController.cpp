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
#include "ores.qt/CurrencyController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CurrencyDetailDialog.hpp"
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/currency_changed_event.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view currency_event_name =
    eventing::domain::event_traits<refdata::eventing::currency_changed_event>::name;
}

CurrencyController::CurrencyController(QMainWindow* mainWindow,
                                       QMdiArea* mdiArea,
                                       ClientManager* clientManager,
                                       ImageCache* imageCache,
                                       ChangeReasonCache* changeReasonCache,
                                       const QString& username,
                                       BadgeCache* badgeCache,
                                       QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, currency_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "CurrencyController created";
}

void CurrencyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "currencies");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CurrencyMdiWindow(clientManager_, username_, badgeCache_, imageCache_);

    // Connect signals
    connect(
        listWindow_, &CurrencyMdiWindow::statusChanged, this, &CurrencyController::statusMessage);
    connect(
        listWindow_, &CurrencyMdiWindow::errorOccurred, this, &CurrencyController::errorMessage);
    connect(listWindow_,
            &CurrencyMdiWindow::showCurrencyDetails,
            this,
            &CurrencyController::onShowDetails);
    connect(listWindow_,
            &CurrencyMdiWindow::addNewRequested,
            this,
            &CurrencyController::onAddNewRequested);
    connect(listWindow_,
            &CurrencyMdiWindow::showCurrencyHistory,
            this,
            &CurrencyController::onShowHistory);
    connect(listWindow_,
            &CurrencyMdiWindow::showRoundingTypesRequested,
            this,
            &CurrencyController::showRoundingTypesRequested);
    connect(listWindow_,
            &CurrencyMdiWindow::showMonetaryNaturesRequested,
            this,
            &CurrencyController::showMonetaryNaturesRequested);
    connect(listWindow_,
            &CurrencyMdiWindow::showMarketTiersRequested,
            this,
            &CurrencyController::showMarketTiersRequested);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Currencies");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));
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
            [self = QPointer<CurrencyController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Currency list window created";
}

void CurrencyController::closeAllWindows() {
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

void CurrencyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CurrencyController::onShowDetails(const refdata::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << currency.iso_code;
    showDetailWindow(currency);
}

void CurrencyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency requested";
    showAddWindow();
}


void CurrencyController::onShowHistory(const refdata::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << currency.iso_code;
    showHistoryWindow(QString::fromStdString(currency.iso_code));
}

void CurrencyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new currency";

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CurrencyDetailDialog::statusMessage,
            this,
            &CurrencyController::statusMessage);
    connect(
        detailDialog, &CurrencyDetailDialog::errorMessage, this, &CurrencyController::errorMessage);
    connect(detailDialog,
            &CurrencyDetailDialog::currencySaved,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyController::showDetailWindow(const refdata::domain::currency& currency) {

    const QString identifier = QString::fromStdString(currency.iso_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << currency.iso_code;

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCurrency(currency);

    connect(detailDialog,
            &CurrencyDetailDialog::statusMessage,
            this,
            &CurrencyController::statusMessage);
    connect(
        detailDialog, &CurrencyDetailDialog::errorMessage, this, &CurrencyController::errorMessage);
    connect(detailDialog,
            &CurrencyDetailDialog::currencySaved,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CurrencyDetailDialog::currencyDeleted,
            this,
            [self = QPointer<CurrencyController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for currency: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog("ores.refdata.currency", code.toStdString(), clientManager_, mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CurrencyController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CurrencyController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onOpenHistoryVersion(entityId, version);
            });

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Currency History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CurrencyController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CurrencyController::onOpenVersion(
    const refdata::domain::currency& currency,
    int versionNumber,
    const std::vector<refdata::domain::currency>& fullHistory) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for currency: " << currency.iso_code;

    const QString code = QString::fromStdString(currency.iso_code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    // A single version-nav toolbar (first/prev/next/last) only means
    // something if the dialog has the *full* history to navigate, not
    // just this one version.
    if (!fullHistory.empty()) {
        detailDialog->setHistory(fullHistory, versionNumber);
    } else {
        detailDialog->setCurrency(currency);
        detailDialog->setReadOnly(true, versionNumber);
    }
    connect(detailDialog,
            &CurrencyDetailDialog::revertRequested,
            this,
            &CurrencyController::onRevertVersion);

    connect(detailDialog,
            &CurrencyDetailDialog::statusMessage,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CurrencyDetailDialog::errorMessage,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CurrencyController::onRevertVersion(const refdata::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), info) << "Reverting currency to version: " << currency.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_currency = currency;
    reverted_currency.version = 0;
    detailDialog->setCurrency(reverted_currency);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CurrencyDetailDialog::statusMessage,
            this,
            &CurrencyController::statusMessage);
    connect(
        detailDialog, &CurrencyDetailDialog::errorMessage, this, &CurrencyController::errorMessage);
    connect(detailDialog,
            &CurrencyDetailDialog::currencySaved,
            this,
            [self = QPointer<CurrencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Currency reverted: " << code.toStdString();
                emit self->statusMessage(QString("Currency '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Currency: %1").arg(QString::fromStdString(currency.iso_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CurrencyController::fetchCurrencyHistory(
    const QString& isoCode, std::function<void(std::vector<refdata::domain::currency>)> callback) {
    refdata::messaging::get_currency_history_request request;
    request.iso_code = isoCode.toStdString();

    QPointer<CurrencyController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run(
        [clientManager, request = std::move(request)]() -> std::vector<refdata::domain::currency> {
            if (!clientManager || !clientManager->isConnected())
                return {};
            auto result = clientManager->process_authenticated_request(std::move(request));
            if (!result || !result->success)
                return {};
            return std::move(result->history);
        });

    auto* watcher = new QFutureWatcher<std::vector<refdata::domain::currency>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<refdata::domain::currency>>::finished,
            this,
            [self, watcher, callback = std::move(callback)]() mutable {
                auto history = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                callback(std::move(history));
            });
    watcher->setFuture(future);
}

void CurrencyController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CurrencyController> self = this;
    fetchCurrencyHistory(
        entityId, [self, entityId, versionNumber](std::vector<refdata::domain::currency> history) {
            if (!self)
                return;
            const auto it = std::find_if(
                history.begin(), history.end(),
                [&](const auto& c) { return c.version == versionNumber; });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onOpenVersion(*it, versionNumber, history);
        });
}

void CurrencyController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CurrencyController> self = this;
    fetchCurrencyHistory(
        entityId, [self, entityId, versionNumber](std::vector<refdata::domain::currency> history) {
            if (!self)
                return;
            const auto it = std::find_if(
                history.begin(), history.end(),
                [&](const auto& c) { return c.version == versionNumber; });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onRevertVersion(*it);
        });
}

EntityListMdiWindow* CurrencyController::listWindow() const {
    return listWindow_;
}

void CurrencyController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
