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
#ifndef ORES_QT_PUBLICATION_HISTORY_DIALOG_HPP
#define ORES_QT_PUBLICATION_HISTORY_DIALOG_HPP

#include <QDialog>
#include <QTableView>
#include <QVBoxLayout>
#include <QAbstractTableModel>
#include <QFutureWatcher>
#include <memory>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/publication.hpp"

namespace ores::qt {

/**
 * @brief Table model for displaying publication history.
 */
class PublicationHistoryModel : public QAbstractTableModel {
    Q_OBJECT

public:
    explicit PublicationHistoryModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void setPublications(const std::vector<dq::domain::publication>& publications);
    void clear();

private:
    enum Column {
        PublishedAt,
        DatasetCode,
        Mode,
        TargetTable,
        Inserted,
        Skipped,
        Deleted,
        PublishedBy,
        ColumnCount
    };

    std::vector<dq::domain::publication> publications_;
};

/**
 * @brief Dialog for displaying publication history.
 *
 * Shows a table of all publication events with timestamps, dataset codes,
 * modes, record counts, and who published.
 */
class PublicationHistoryDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.publication_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PublicationHistoryDialog(ClientManager* clientManager,
                                      QWidget* parent = nullptr);
    ~PublicationHistoryDialog() override;

    /**
     * @brief Refresh the publication list from the server.
     */
    void refresh();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);

private slots:
    void onPublicationsLoaded();

private:
    void setupUi();

    QTableView* tableView_;
    PublicationHistoryModel* model_;
    ClientManager* clientManager_;

    struct FetchResult {
        bool success;
        std::vector<dq::domain::publication> publications;
    };

    QFutureWatcher<FetchResult>* watcher_;
};

}

#endif
