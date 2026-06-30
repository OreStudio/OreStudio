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
#ifndef ORES_QT_HISTORY_DIALOG_BASE_HPP
#define ORES_QT_HISTORY_DIALOG_BASE_HPP

#include "ores.qt/ClientManager.hpp"
#include "ores.qt/export.hpp"
#include <QFuture>
#include <QFutureWatcher>
#include <QList>
#include <QPair>
#include <QPointer>
#include <QString>
#include <QStringList>
#include <QWidget>
#include <QtConcurrent>
#include <expected>
#include <optional>
#include <string>

class QAction;
class QLabel;
class QPushButton;
class QTableWidget;
class QToolBar;

namespace ores::qt {

/**
 * @brief Base class for all history dialogs.
 *
 * Owns the machinery every entity history dialog shares:
 *
 * - statusChanged / errorOccurred signals.
 * - The Reload / Open / Revert toolbar and its action states.
 * - Version-list population and selection handling.
 * - The changes-tab rendering flow, which calls the calculateDiffAt()
 *   template method; once history responses carry server-computed
 *   diffs, implementations collapse into rendering the provided rows.
 * - checkString / checkInt / checkBool field-diff helpers so all
 *   dialogs format values identically in the interim.
 * - Async request plumbing (runHistoryRequest) and watcher cleanup.
 * - markAsStale() — reloads by default; code() identifies the entity.
 *
 * Derived dialogs call initializeHistoryUi() once their Ui is set up,
 * and override the protected hooks for what is genuinely
 * entity-specific. Hooks have safe defaults so existing derived
 * classes migrate incrementally.
 */
class ORES_QT_API HistoryDialogBase : public QWidget {
    Q_OBJECT

public:
    /**
     * @brief Field-level diff rows: (field, (old value, new value)).
     */
    using DiffResult = QList<QPair<QString, QPair<QString, QString>>>;

    using QWidget::QWidget;
    ~HistoryDialogBase() override;

    /**
     * @brief Reloads the history; called when a server-side change
     * event arrives for this entity.
     */
    virtual void markAsStale();

    /**
     * @brief Returns the entity identifier displayed in this dialog;
     * used by controllers to filter notifications.
     */
    [[nodiscard]] virtual QString code() const {
        return {};
    }

    /**
     * @brief Loads (or reloads) the history from the server.
     */
    virtual void loadHistory() {}

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

protected:
    /**
     * @brief One row of the version list, rendered as strings.
     *
     * The version number always occupies column zero; cells supplies
     * the remaining columns in the order the dialog's version list
     * defines them (dialogs vary: some add change reason and
     * commentary columns, others an enabled flag).
     */
    struct VersionRow {
        int version{};
        QStringList cells;
    };

    /**
     * @brief The widgets the base machinery drives, owned by the
     * derived dialog's Ui. changesTable, titleLabel and closeButton
     * may be null when the dialog lacks the corresponding element.
     */
    struct HistoryWidgets {
        QTableWidget* versionList{};
        QTableWidget* changesTable{};
        QLabel* titleLabel{};
        QPushButton* closeButton{};
    };

    /**
     * @brief Wires the shared machinery: toolbar, table behaviour,
     * selection handling, close button. Call once after setupUi().
     */
    void initializeHistoryUi(const HistoryWidgets& widgets);

    /**
     * @brief Repopulates the version list from the hooks and selects
     * the newest version. Call from the derived loadHistory()
     * success path; emits statusChanged.
     */
    void historyLoaded();

    /**
     * @brief Reports a history load failure: logs, emits
     * errorOccurred, and shows the standard message box.
     */
    void historyLoadFailed(const QString& error_message);

    /**
     * @brief Currently selected version-list row, or -1.
     */
    [[nodiscard]] int selectedVersionIndex() const;

    // ---- Hooks: override what is entity-specific. -------------------

    /**
     * @brief Number of versions currently loaded.
     */
    [[nodiscard]] virtual int historySize() const {
        return 0;
    }

    /**
     * @brief The version-list row for the given index (0 = newest).
     */
    [[nodiscard]] virtual VersionRow versionRow(int index) const;

    /**
     * @brief Title shown above the version list once loaded.
     */
    [[nodiscard]] virtual QString historyTitle() const {
        return {};
    }

    /**
     * @brief Field-level diff between two loaded versions. The
     * interim seam: once server-side diffs land this renders the
     * response rows and eventually disappears.
     */
    [[nodiscard]] virtual DiffResult calculateDiffAt(int current_index, int previous_index) const;

    /**
     * @brief Renders the full-details panel for the given version.
     */
    virtual void displayFullDetails(int index);

    /**
     * @brief Opens the given version read-only (Open action /
     * double-click). Derived dialogs emit their typed signal.
     */
    virtual void openVersionAt(int index);

    /**
     * @brief Reverts to the version at the given index, after the
     * base has confirmed with the user. Derived dialogs emit their
     * typed signal.
     */
    virtual void revertToVersionAt(int index);

    /**
     * @brief Optional widget rendering for a changes-table cell (e.g.
     * flag icons); return null for the default text item.
     */
    virtual QWidget* changeCellWidget(const QString& field, const QString& value);

    // ---- Interim diff helpers. --------------------------------------

    static void checkString(DiffResult& diffs,
                            const QString& field,
                            const std::string& current,
                            const std::string& previous);
    static void checkString(DiffResult& diffs,
                            const QString& field,
                            const std::optional<std::string>& current,
                            const std::optional<std::string>& previous);
    static void checkInt(DiffResult& diffs, const QString& field, int current, int previous);
    static void checkInt(DiffResult& diffs,
                         const QString& field,
                         const std::optional<int>& current,
                         const std::optional<int>& previous);
    static void checkDouble(DiffResult& diffs, const QString& field, double current, double previous);
    static void checkBool(DiffResult& diffs, const QString& field, bool current, bool previous);

    /**
     * @brief Runs an authenticated request off the UI thread and
     * delivers the response (or a load failure) back on it.
     *
     * @param request The typed request message.
     * @param on_success Invoked with the moved response on success.
     */
    template <typename Request, typename OnSuccess>
    void runHistoryRequest(ClientManager* client_manager, Request request, OnSuccess on_success) {
        using Response = typename decltype(client_manager->process_authenticated_request(
            std::move(request)))::value_type;
        using Result = std::expected<Response, std::string>;

        QPointer<HistoryDialogBase> self = this;
        QFuture<Result> future = QtConcurrent::run(
            [self, client_manager, request = std::move(request)]() mutable -> Result {
                if (!client_manager || !client_manager->isConnected())
                    return std::unexpected("Disconnected from server");
                auto result = client_manager->process_authenticated_request(std::move(request));
                if (!result)
                    return std::unexpected(result.error());
                return std::move(*result);
            });

        auto* watcher = new QFutureWatcher<Result>(this);
        connect(watcher,
                &QFutureWatcher<Result>::finished,
                this,
                [self, watcher, on_success = std::move(on_success)]() mutable {
                    if (!self)
                        return;
                    auto result = watcher->result();
                    watcher->deleteLater();
                    if (!result) {
                        self->historyLoadFailed(QString::fromStdString(result.error()));
                        return;
                    }
                    on_success(std::move(*result));
                });
        watcher->setFuture(future);
    }

private slots:
    void onReloadClicked();
    void onOpenClicked();
    void onRevertClicked();

private:
    void setupToolbar();
    void displayChangesTab(int version_index);
    void onVersionSelectedRow(int row);
    void updateActionStates();

    HistoryWidgets widgets_;
    QToolBar* toolBar_{};
    QAction* reloadAction_{};
    QAction* openAction_{};
    QAction* revertAction_{};
};

}

#endif
