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
#include <optional>
#include <QFile>
#include <QFileInfo>
#include <QFileDialog>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.qt/ImportTradeDialog.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.ore/xml/exporter.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BookMdiWindow::BookMdiWindow(
    ClientManager* clientManager,
    ImageCache* imageCache,
    const QString& username,
    BadgeCache* badgeCache,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      username_(username),
      badgeCache_(badgeCache),
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
      importAction_(nullptr),
      exportXmlAction_(nullptr) {

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
    layout->addWidget(loadingBar());

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
            &EntityListMdiWindow::reload);

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

    exportXmlAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ExportOre, IconUtils::DefaultIconColor),
        tr("Export"));
    exportXmlAction_->setToolTip(
        tr("Export all trades in the selected book to an ORE portfolio XML file"));
    exportXmlAction_->setEnabled(false);
    connect(exportXmlAction_, &QAction::triggered, this,
            &BookMdiWindow::exportToXml);
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
    delegate->set_badge_color_resolver(2, [cache = badgeCache_](const QString& value) -> badge_color_pair {
        static const badge_color_pair default_gray{QColor(0x6B, 0x72, 0x80), Qt::white};
        if (!cache) return default_gray;
        auto* def = cache->resolve("book_status", value.toStdString());
        if (!def) return default_gray;
        return {QColor(QString::fromStdString(def->background_colour)),
                QColor(QString::fromStdString(def->text_colour))};
    });
    delegate->set_badge_color_resolver(3, [cache = badgeCache_](const QString& value) -> badge_color_pair {
        static const badge_color_pair default_gray{QColor(0x6B, 0x72, 0x80), Qt::white};
        if (!cache) return default_gray;
        auto* def = cache->resolve("book_type", value.toStdString());
        if (!def) return default_gray;
        return {QColor(QString::fromStdString(def->background_colour)),
                QColor(QString::fromStdString(def->text_colour))};
    });
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
    connectModel(model_);

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

void BookMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading books";
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
    exportXmlAction_->setEnabled(hasSelection);
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
            {}, clientManager_, username_, this);

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

void BookMdiWindow::exportToXml() {
    BOOST_LOG_SEV(lg(), debug) << "Export to ORE XML triggered";

    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Export: no book selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* book = model_->getBook(sourceIndex.row());
    if (!book) {
        BOOST_LOG_SEV(lg(), warn) << "Export: could not get book";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot export trades while disconnected."));
        return;
    }

    const std::string book_id = boost::uuids::to_string(book->id);
    const std::string book_name = book->name;

    emit statusChanged(tr("Fetching trades for export..."));

    QPointer<BookMdiWindow> self = this;

    struct ExportTaskResult {
        bool success = false;
        std::string error;
        std::string xml;
        std::size_t trade_count = 0;
    };

    auto task = [self, book_id]() -> ExportTaskResult {
        ExportTaskResult result;
        if (!self) return result;

        trading::messaging::export_portfolio_request req;
        req.book_id = book_id;

        auto r = self->clientManager_->process_authenticated_request(
            std::move(req));
        if (!r) {
            result.error = "Failed to communicate with server";
            return result;
        }
        if (!r->success) {
            result.error = r->message;
            return result;
        }
        if (r->items.empty()) {
            result.success = true;
            return result;
        }

        try {
            result.xml = ore::xml::exporter::export_portfolio(r->items);
            result.trade_count = r->items.size();
            result.success = true;
        } catch (const std::exception& e) {
            result.error = e.what();
        }
        return result;
    };

    auto* watcher = new QFutureWatcher<ExportTaskResult>(this);
    connect(watcher, &QFutureWatcher<ExportTaskResult>::finished,
            this, [self, watcher, book_name]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Export failed: " << result.error;
            MessageBoxHelper::critical(self, tr("Export Error"),
                tr("Failed to export portfolio:\n%1")
                .arg(QString::fromStdString(result.error)));
            emit self->statusChanged(tr("Export failed"));
            return;
        }

        if (result.trade_count == 0) {
            MessageBoxHelper::information(self, tr("No Trades"),
                tr("Book '%1' has no trades to export.")
                .arg(QString::fromStdString(book_name)));
            emit self->statusChanged(tr("Export: no trades found"));
            return;
        }

        const QString defaultName =
            QString::fromStdString("portfolio_" + book_name + ".xml");
        const QString fileName = QFileDialog::getSaveFileName(
            self,
            tr("Save ORE Portfolio XML"),
            defaultName,
            tr("ORE Portfolio Files (*.xml);;All Files (*)"));

        if (!self) return;

        if (fileName.isEmpty()) {
            BOOST_LOG_SEV(lg(), debug) << "Export: save dialog cancelled";
            emit self->statusChanged(tr("Export cancelled"));
            return;
        }

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Cannot open file for writing: "
                                       << fileName.toStdString();
            MessageBoxHelper::critical(self, tr("Export Error"),
                tr("Cannot write to file:\n%1").arg(fileName));
            emit self->statusChanged(tr("Export failed"));
            return;
        }

        file.write(result.xml.data(), static_cast<qint64>(result.xml.size()));
        file.close();

        BOOST_LOG_SEV(lg(), info) << "Exported " << result.trade_count
                                  << " trades to: " << fileName.toStdString();
        emit self->statusChanged(
            tr("Exported %1 trades to %2")
            .arg(result.trade_count)
            .arg(QFileInfo(fileName).fileName()));
        MessageBoxHelper::information(self, tr("Export Complete"),
            tr("Successfully exported %1 trade(s) to:\n%2")
            .arg(result.trade_count)
            .arg(fileName));
    });

    watcher->setFuture(QtConcurrent::run(task));
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
        for (const auto& id : ids)
            request.ids.push_back(boost::uuids::to_string(id));
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        for (std::size_t i = 0; i < ids.size(); ++i) {
            std::string code = (i < codes.size()) ? codes[i] : "";
            results.push_back({ids[i], code, response_result->success, response_result->message});
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
