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
#include "ores.qt/PortfolioController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PortfolioDetailDialog.hpp"
#include "ores.qt/PortfolioMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/portfolio_changed_event.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view portfolio_event_name =
    eventing::domain::event_traits<refdata::eventing::portfolio_changed_event>::name;
}

PortfolioController::PortfolioController(QMainWindow* mainWindow,
                                         QMdiArea* mdiArea,
                                         ClientManager* clientManager,
                                         ImageCache* imageCache,
                                         ChangeReasonCache* changeReasonCache,
                                         const QString& username,
                                         BadgeCache* badgeCache,
                                         QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, portfolio_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "PortfolioController created";
}

void PortfolioController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "portfolios");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PortfolioMdiWindow(clientManager_, username_, badgeCache_, imageCache_);

    // Connect signals
    connect(
        listWindow_, &PortfolioMdiWindow::statusChanged, this, &PortfolioController::statusMessage);
    connect(
        listWindow_, &PortfolioMdiWindow::errorOccurred, this, &PortfolioController::errorMessage);
    connect(listWindow_,
            &PortfolioMdiWindow::showPortfolioDetails,
            this,
            &PortfolioController::onShowDetails);
    connect(listWindow_,
            &PortfolioMdiWindow::addNewRequested,
            this,
            &PortfolioController::onAddNewRequested);
    connect(listWindow_,
            &PortfolioMdiWindow::showPortfolioHistory,
            this,
            &PortfolioController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Portfolios");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));
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
            [self = QPointer<PortfolioController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Portfolio list window created";
}

void PortfolioController::closeAllWindows() {
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

void PortfolioController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PortfolioController::onShowDetails(const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << portfolio.name;
    showDetailWindow(portfolio);
}

void PortfolioController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new portfolio requested";
    showAddWindow();
}

void PortfolioController::openAdd() {
    showAddWindow();
}
void PortfolioController::openAddWithParent(boost::uuids::uuid parentPortfolioId) {
    showAddWindow(parentPortfolioId);
}
void PortfolioController::openEdit(const refdata::domain::portfolio& portfolio) {
    showDetailWindow(portfolio);
}
void PortfolioController::openHistory(const refdata::domain::portfolio& portfolio) {
    showHistoryWindow(portfolio);
}

void PortfolioController::onShowHistory(const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << portfolio.name;
    showHistoryWindow(portfolio);
}

void PortfolioController::showAddWindow(boost::uuids::uuid parentPortfolioId) {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new portfolio";

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    if (!parentPortfolioId.is_nil()) {
        refdata::domain::portfolio prefilled;
        prefilled.parent_portfolio_id = parentPortfolioId;
        detailDialog->setPortfolio(prefilled);
    }
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &PortfolioDetailDialog::statusMessage,
            this,
            &PortfolioController::statusMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::errorMessage,
            this,
            &PortfolioController::errorMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::portfolioSaved,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Portfolio saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Portfolio");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PortfolioController::showDetailWindow(const refdata::domain::portfolio& portfolio) {

    const QString identifier = QString::fromStdString(portfolio.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << portfolio.name;

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setPortfolio(portfolio);

    connect(detailDialog,
            &PortfolioDetailDialog::statusMessage,
            this,
            &PortfolioController::statusMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::errorMessage,
            this,
            &PortfolioController::errorMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::portfolioSaved,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Portfolio saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &PortfolioDetailDialog::portfolioDeleted,
            this,
            [self = QPointer<PortfolioController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Portfolio deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Portfolio: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<PortfolioController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PortfolioController::showHistoryWindow(const refdata::domain::portfolio& portfolio) {
    const QString code = QString::fromStdString(portfolio.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for portfolio: " << portfolio.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << portfolio.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << portfolio.name;

    const QString entityId = QString::fromStdString(boost::uuids::to_string(portfolio.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::portfolio{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<PortfolioController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<PortfolioController>(this)](
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
    historyWindow->setWindowTitle(QString("Portfolio History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<PortfolioController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PortfolioController::onOpenVersion(const refdata::domain::portfolio& portfolio,
                                        int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for portfolio: " << portfolio.name;

    const QString code = QString::fromStdString(portfolio.name);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setPortfolio(portfolio);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &PortfolioDetailDialog::statusMessage,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &PortfolioDetailDialog::errorMessage,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Portfolio: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PortfolioController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PortfolioController::fetchPortfolioHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::portfolio>, QString>)> callback) {
    refdata::messaging::get_portfolio_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::portfolio>, QString>;

    QPointer<PortfolioController> self = this;
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

void PortfolioController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<PortfolioController> self = this;
    fetchPortfolioHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::portfolio>, QString> result) {
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

void PortfolioController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<PortfolioController> self = this;
    fetchPortfolioHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::portfolio>, QString> result) {
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

void PortfolioController::onRevertVersion(const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), info) << "Reverting portfolio to version: " << portfolio.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_portfolio = portfolio;
    reverted_portfolio.version = 0;
    detailDialog->setPortfolio(reverted_portfolio);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &PortfolioDetailDialog::statusMessage,
            this,
            &PortfolioController::statusMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::errorMessage,
            this,
            &PortfolioController::errorMessage);
    connect(detailDialog,
            &PortfolioDetailDialog::portfolioSaved,
            this,
            [self = QPointer<PortfolioController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Portfolio reverted: " << code.toStdString();
                emit self->statusMessage(QString("Portfolio '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Portfolio: %1").arg(QString::fromStdString(portfolio.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PortfolioController::listWindow() const {
    return listWindow_;
}

void PortfolioController::notifyOpenDialogs(const QStringList& entityIds) {
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
