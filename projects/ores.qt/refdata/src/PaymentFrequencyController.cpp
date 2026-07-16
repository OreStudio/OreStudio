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
#include "ores.qt/PaymentFrequencyController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PaymentFrequencyDetailDialog.hpp"
#include "ores.qt/PaymentFrequencyMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/payment_frequency_changed_event.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view payment_frequency__event_name =
    eventing::domain::event_traits<refdata::eventing::payment_frequency_changed_event>::name;
}

PaymentFrequencyController::PaymentFrequencyController(QMainWindow* mainWindow,
                                                       QMdiArea* mdiArea,
                                                       ClientManager* clientManager,
                                                       ChangeReasonCache* changeReasonCache,
                                                       const QString& username,
                                                       BadgeCache* badgeCache,
                                                       QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, payment_frequency__event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PaymentFrequencyController created";
}

void PaymentFrequencyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "payment_frequencies");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PaymentFrequencyMdiWindow(clientManager_, username_, badgeCache_);

    // Connect signals
    connect(listWindow_,
            &PaymentFrequencyMdiWindow::statusChanged,
            this,
            &PaymentFrequencyController::statusMessage);
    connect(listWindow_,
            &PaymentFrequencyMdiWindow::errorOccurred,
            this,
            &PaymentFrequencyController::errorMessage);
    connect(listWindow_,
            &PaymentFrequencyMdiWindow::showFrequencyDetails,
            this,
            &PaymentFrequencyController::onShowDetails);
    connect(listWindow_,
            &PaymentFrequencyMdiWindow::addNewRequested,
            this,
            &PaymentFrequencyController::onAddNewRequested);
    connect(listWindow_,
            &PaymentFrequencyMdiWindow::showFrequencyHistory,
            this,
            &PaymentFrequencyController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Payment Frequencies");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));
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
            [self = QPointer<PaymentFrequencyController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Payment Frequency list window created";
}

void PaymentFrequencyController::closeAllWindows() {
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

void PaymentFrequencyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PaymentFrequencyController::onShowDetails(
    const refdata::domain::payment_frequency& payment_frequency_) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << payment_frequency_.code;
    showDetailWindow(payment_frequency_);
}

void PaymentFrequencyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new payment frequency requested";
    showAddWindow();
}


void PaymentFrequencyController::onShowHistory(
    const refdata::domain::payment_frequency& payment_frequency_) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << payment_frequency_.code;
    showHistoryWindow(QString::fromStdString(payment_frequency_.code));
}

void PaymentFrequencyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new payment frequency";

    auto* detailDialog = new PaymentFrequencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &PaymentFrequencyDetailDialog::statusMessage,
            this,
            &PaymentFrequencyController::statusMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::errorMessage,
            this,
            &PaymentFrequencyController::errorMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::payment_frequency_Saved,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Payment Frequency saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Payment Frequency");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PaymentFrequencyController::showDetailWindow(
    const refdata::domain::payment_frequency& payment_frequency_) {

    const QString identifier = QString::fromStdString(payment_frequency_.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << payment_frequency_.code;

    auto* detailDialog = new PaymentFrequencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setFrequency(payment_frequency_);

    connect(detailDialog,
            &PaymentFrequencyDetailDialog::statusMessage,
            this,
            &PaymentFrequencyController::statusMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::errorMessage,
            this,
            &PaymentFrequencyController::errorMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::payment_frequency_Saved,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Payment Frequency saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::payment_frequency_Deleted,
            this,
            [self = QPointer<PaymentFrequencyController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Payment Frequency deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Payment Frequency: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<PaymentFrequencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PaymentFrequencyController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for payment frequency: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::payment_frequency{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](
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
    historyWindow->setWindowTitle(QString("Payment Frequency History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<PaymentFrequencyController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PaymentFrequencyController::onOpenVersion(
    const refdata::domain::payment_frequency& payment_frequency_, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for payment frequency: " << payment_frequency_.code;

    const QString code = QString::fromStdString(payment_frequency_.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PaymentFrequencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setFrequency(payment_frequency_);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &PaymentFrequencyDetailDialog::statusMessage,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::errorMessage,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Payment Frequency: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PaymentFrequencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PaymentFrequencyController::fetchPaymentFrequencyHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::payment_frequency>, QString>)>
        callback) {
    refdata::messaging::get_payment_frequency_history_request request;
    request.code = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::payment_frequency>, QString>;

    QPointer<PaymentFrequencyController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->history);
    });

    auto* watcher = new QFutureWatcher<FetchResult>(this);
    connect(watcher,
            &QFutureWatcher<FetchResult>::finished,
            this,
            [self, watcher, callback = std::move(callback)]() mutable {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                callback(std::move(result));
            });
    watcher->setFuture(future);
}

void PaymentFrequencyController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<PaymentFrequencyController> self = this;
    fetchPaymentFrequencyHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::payment_frequency>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onOpenVersion(*it, versionNumber);
        });
}

void PaymentFrequencyController::onRevertHistoryVersion(const QString& entityId,
                                                        int versionNumber) {
    QPointer<PaymentFrequencyController> self = this;
    fetchPaymentFrequencyHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::payment_frequency>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onRevertVersion(*it);
        });
}

void PaymentFrequencyController::onRevertVersion(
    const refdata::domain::payment_frequency& payment_frequency_) {
    BOOST_LOG_SEV(lg(), info) << "Reverting payment frequency to version: "
                              << payment_frequency_.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PaymentFrequencyDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setBadgeCache(badgeCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_payment_frequency_ = payment_frequency_;
    reverted_payment_frequency_.version = 0;
    detailDialog->setFrequency(reverted_payment_frequency_);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &PaymentFrequencyDetailDialog::statusMessage,
            this,
            &PaymentFrequencyController::statusMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::errorMessage,
            this,
            &PaymentFrequencyController::errorMessage);
    connect(detailDialog,
            &PaymentFrequencyDetailDialog::payment_frequency_Saved,
            this,
            [self = QPointer<PaymentFrequencyController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Payment Frequency reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Payment Frequency '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Payment Frequency: %1")
                                     .arg(QString::fromStdString(payment_frequency_.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PaymentFrequencyController::listWindow() const {
    return listWindow_;
}

void PaymentFrequencyController::notifyOpenDialogs(const QStringList& entityIds) {
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
