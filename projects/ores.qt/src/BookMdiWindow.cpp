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
#include "ores.qt/BookMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/BadgeColors.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.qt/ImportTradeDialog.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BookMdiWindow::BookMdiWindow(
    ClientManager* clientManager,
    ImageCache* imageCache,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr),
      importAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void BookMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void BookMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &BookMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new book"));
    connect(addAction_, &QAction::triggered, this,
            &BookMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected book"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &BookMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected book"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &BookMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View book history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &BookMdiWindow::viewHistorySelected);

    toolbar_->addSeparator();

    importAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ImportOre, IconUtils::DefaultIconColor),
        tr("Import"));
    importAction_->setToolTip(
        tr("Import trades from an ORE portfolio XML file into the selected book"));
    importAction_->setEnabled(false);
    connect(importAction_, &QAction::triggered, this,
            &BookMdiWindow::importTrades);
}

void BookMdiWindow::setupTable() {
    model_ = new ClientBookModel(clientManager_, imageCache_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    using cs = column_style;
    auto* delegate = new EntityItemDelegate({
        cs::text_left,      // Name
        cs::mono_center,    // LedgerCcy
        cs::badge_centered, // BookStatus
        cs::badge_centered, // IsTradingBook (Trading/Banking)
        cs::text_left,      // CostCenter
        cs::mono_center,    // Version
        cs::text_left,      // ModifiedBy
        cs::mono_left       // RecordedAt
    }, tableView_);
    delegate->set_badge_color_resolver(resolve_book_badge_color);
    tableView_->setItemDelegate(delegate);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_, "BookListWindow",
        {}, {900, 400}, 1);
}

void BookMdiWindow::setupConnections() {
    connect(model_, &ClientBookModel::dataLoaded,
            this, &BookMdiWindow::onDataLoaded);
    connect(model_, &ClientBookModel::loadError,
            this, &BookMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &BookMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &BookMdiWindow::onDoubleClicked);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
}

void BookMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading books";
    clearStaleIndicator();
    emit statusChanged(tr("Loading books..."));
    model_->refresh();
}

void BookMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 books").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void BookMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void BookMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void BookMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* book = model_->getBook(sourceIndex.row())) {
        emit showBookDetails(*book);
    }
}

void BookMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
    importAction_->setEnabled(hasSelection);
}

void BookMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new book requested";
    emit addNewRequested();
}

void BookMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* book = model_->getBook(sourceIndex.row())) {
        emit showBookDetails(*book);
    }
}

void BookMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* book = model_->getBook(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showBookHistory for code: "
                                   << book->name;
        emit showBookHistory(*book);
    }
}

void BookMdiWindow::importTrades() {
    BOOST_LOG_SEV(lg(), debug) << "Import trades action triggered";

    // Get selected book
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Import trades: no book selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* book = model_->getBook(sourceIndex.row());
    if (!book) {
        BOOST_LOG_SEV(lg(), warn) << "Import trades: could not get book";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot import trades while disconnected."));
        return;
    }

    // Ask the user to select an ORE portfolio XML file
    const QString fileName = QFileDialog::getOpenFileName(
        this,
        tr("Select ORE Portfolio XML File to Import"),
        QString(),
        tr("ORE Portfolio Files (*.xml);;All Files (*)"));

    if (fileName.isEmpty()) {
        BOOST_LOG_SEV(lg(), debug) << "Import trades: user cancelled file dialog";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Selected portfolio file for import: "
                              << fileName.toStdString();

    try {
        const std::filesystem::path path(fileName.toStdString());
        const auto items = ore::xml::importer::import_portfolio_with_context(path);

        if (items.empty()) {
            emit statusChanged(tr("Import cancelled - no trades found in file"));
            MessageBoxHelper::information(this, tr("No Trades"),
                tr("The selected file contains no trades."));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Parsed " << items.size()
                                  << " trades from portfolio file";
        emit statusChanged(QString("Found %1 trades - opening import dialog...")
            .arg(items.size()));

        // Use just the filename as the display label
        const QFileInfo fileInfo(fileName);
        auto* dialog = new ImportTradeDialog(
            *book, items, fileInfo.fileName(),
            clientManager_, username_, this);

        connect(dialog, &ImportTradeDialog::importCompleted,
                this, [this](int success_count, int total_count) {
            BOOST_LOG_SEV(lg(), info)
                << "Trade import complete: " << success_count
                << " of " << total_count << " trades imported";

            model_->refresh();

            if (success_count > 0) {
                emit statusChanged(
                    tr("Successfully imported %1 of %2 trades")
                    .arg(success_count).arg(total_count));
                if (success_count < total_count) {
                    MessageBoxHelper::warning(this, tr("Partial Import"),
                        tr("Imported %1 of %2 trades. Check the log for details.")
                        .arg(success_count).arg(total_count));
                } else {
                    MessageBoxHelper::information(this, tr("Import Complete"),
                        tr("Successfully imported %1 trade(s).")
                        .arg(success_count));
                }
            } else {
                emit statusChanged(tr("Import failed - no trades imported"));
                MessageBoxHelper::warning(this, tr("Import Failed"),
                    tr("Failed to import trades. Check the log for details."));
            }
        });

        connect(dialog, &ImportTradeDialog::importCancelled,
                this, [this]() {
            BOOST_LOG_SEV(lg(), debug) << "Trade import cancelled by user";
            emit statusChanged(tr("Import cancelled"));
        });

        dialog->open();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error importing portfolio XML: " << e.what();
        MessageBoxHelper::critical(this, tr("Import Error"),
            tr("Failed to import portfolio XML file:\n%1").arg(e.what()));
        emit statusChanged(tr("Import failed"));
    }
}

void BookMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete book while disconnected.");
        return;
    }

    std::vector<boost::uuids::uuid> ids;
    std::vector<std::string> codes;  // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* book = model_->getBook(sourceIndex.row())) {
            ids.push_back(book->id);
            codes.push_back(book->name);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid books to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " books";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete book '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 books?")
            .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Book",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<BookMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<boost::uuids::uuid, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << ids.size() << " books";

        refdata::messaging::delete_book_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_book_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to decompress server response"});
            }
            return results;
        }

        auto response = refdata::messaging::delete_book_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Invalid server response"});
            }
            return results;
        }

        // Match results with codes for display purposes
        for (std::size_t i = 0; i < response->results.size(); ++i) {
            const auto& result = response->results[i];
            std::string code = (i < codes.size()) ? codes[i] : "";
            results.push_back({result.id, code, result.success, result.message});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [id, code, success, message] : results) {
            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Book deleted: " << code;
                success_count++;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Book deletion failed: "
                                           << code << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 book"
                : QString("Successfully deleted %1 books").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "book" : "books")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1, failed to delete %2")
                .arg(success_count)
                .arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
