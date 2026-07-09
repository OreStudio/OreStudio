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
#ifndef ORES_QT_QA_VALIDATION_RUNNER_WIDGET_HPP
#define ORES_QT_QA_VALIDATION_RUNNER_WIDGET_HPP

#include "ores.qt/export.hpp"
#include <QList>
#include <QString>
#include <QWidget>
#include <functional>
#include <memory>
#include <string>
#include <vector>

class QLabel;
class QListWidget;
class QListWidgetItem;
class QMdiArea;
class QToolBar;
class QAction;
class QTabWidget;
class QTextBrowser;
class QUrl;

namespace ores::orgmode::indexing {
class resolver;
}

namespace ores::qt {

/**
 * @brief One step's in-memory state while a scenario is loaded. Each
 * step is its own org heading under =* Steps= (see the =test_scenario=
 * template) — @p title is that heading's title, @p bodyLines its raw
 * instructional text (rendered — markup and all, via =OrgDocRenderer=
 * — in the step detail dialog, not shown as plain unprocessed text).
 */
enum class step_status { pending, pass, fail };

struct qa_step final {
    QString title;
    std::vector<std::string> bodyLines;
    step_status status = step_status::pending;
    QString notes;

    /**
     * @brief Which client instance this step belongs to (e.g. "blue"),
     * for a multi-client scenario — display only (prefixes the step's
     * list entry); the org heading title used to locate/write the
     * step's result is @p title alone, unprefixed.
     */
    QString client;
};

/**
 * @brief The QA Validation Runner: opens a =test_scenario= doc, shows
 * its info and a list of steps (each its own org heading), lets the
 * tester open a step to read its instructions, mark it PASS/FAIL/still
 * pending and write a note, then saves — rewriting the doc's overall
 * =* Results= and each step's =*** Result= child heading in place via
 * =write_scenario_results()= and stamping the compass journal. The
 * scenario's overall outcome is inferred from the step results, not
 * set directly by the tester. Sibling "Story" and "Task" tabs render
 * the driving docs (resolved via the org-roam index and rendered with
 * =OrgDocRenderer=) so the tester doesn't need to leave the app to
 * recover context — =[[id:...][...]]= links inside them are clickable
 * and navigate to whatever they point to, resolved the same way.
 *
 * A regular widget — hosted in an MDI subwindow (via
 * =DetachableMdiSubWindow=) by the plugin that owns it, not a
 * dock, so the tester can freely move/resize it like every other
 * window while running through a test. Step-detail and link-viewer
 * pop-ups follow the same convention (see setMdiArea()) — MDI
 * subwindows, like every other detail dialog in this app, not
 * detached top-level windows.
 */
class ORES_QT_API QaValidationRunnerWidget final : public QWidget {
    Q_OBJECT

public:
    explicit QaValidationRunnerWidget(QWidget* parent = nullptr);

    /**
     * @brief Inject the shared MDI area. Step-detail and link-viewer
     * pop-ups are hosted as MDI subwindows in it, the same as every
     * other detail dialog in the app — not detached top-level windows,
     * which several window managers pin above the whole application
     * (including unrelated ones) regardless of modality.
     */
    void setMdiArea(QMdiArea* mdiArea) {
        mdiArea_ = mdiArea;
    }

public slots:
    /**
     * @brief Parse @p path as a =test_scenario= doc and populate the
     * step list from it. Replaces any previously loaded scenario.
     * @return true on success; emits errorOccurred() and leaves the
     * widget showing its previous (or empty) state on failure.
     */
    bool loadScenario(const QString& path);

    /**
     * @brief Prompt the tester for a =.org= file and load it.
     */
    void promptAndLoadScenario();

    /**
     * @brief Write the current step results back to the loaded
     * scenario doc and stamp the compass journal.
     * @return true on success.
     */
    bool save();

signals:
    void statusMessage(const QString& message);
    void errorOccurred(const QString& message);
    void saved(const QString& path);

private slots:
    void openStepDetail(QListWidgetItem* item);
    void followContextLink(const QUrl& url);

private:
    void rebuildStepList();
    void updateStepItem(int index);
    void updateOverallStatusLabel();
    void stampJournal(const QString& status);
    void loadContextTab(const QString& taskId, const QString& storyId);

    /**
     * @brief Open the org-roam index (=.org-roam.db=, found by walking
     * up from the loaded scenario's path), or nullptr if it isn't
     * found or can't be opened.
     */
    std::unique_ptr<orgmode::indexing::resolver> openResolver() const;

    /**
     * @brief Resolve @p id via @p resolver and render the target doc
     * into @p browser, or a plain-text explanation if @p resolver is
     * null, @p id is empty, or the id doesn't resolve.
     */
    static void
    renderIdInto(orgmode::indexing::resolver* resolver, const QString& id, QTextBrowser* browser);

    QString scenarioPath_;
    QString taskId_;
    QString storyId_;

    /**
     * @brief Bumped every time loadScenario() runs. A step-detail
     * subwindow captures this at open time and checks it before
     * writing back into steps_ — if the tester loads a different
     * scenario while a subwindow from the old one is still open, its
     * writes become no-ops instead of silently landing on the new
     * scenario's step at the same index.
     */
    int loadGeneration_ = 0;

    std::vector<qa_step> steps_;

    /**
     * @brief Grabs the full primary screen after a short delay (so the
     * tester can switch to the window under test first), saves it as a
     * PNG next to the loaded scenario doc, and reports the result via
     * onDone(path) — an empty path means the save failed. Shared by
     * both the scenario-level toolbar action and each step's own
     * screenshot button.
     * @param baseName File stem (before "_<timestamp>.png"); caller
     * supplies the scenario slug, optionally with a step suffix.
     */
    void captureScreenshot(const QString& baseName, const std::function<void(QString)>& onDone);

    QToolBar* toolBar_;
    QAction* openAction_;
    QAction* saveAction_;
    QAction* screenshotAction_;
    QLabel* titleLabel_;
    QLabel* targetDialogLabel_;
    QLabel* overallStatusLabel_;
    QListWidget* stepsList_;

    QTabWidget* tabWidget_;
    QWidget* stepsTab_;
    QTextBrowser* storyBrowser_;
    QTextBrowser* taskBrowser_;

    QMdiArea* mdiArea_ = nullptr;
};

}

#endif
